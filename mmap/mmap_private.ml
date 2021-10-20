(** Additional support for mmap; we want to have: 1) growable files;
   2) ability to track the max used part of the file *)

(* open Util *)

open Bigarray
(* open Mmap_intf *)

type ('a,'b) t = { 
  fd          : Unix.file_descr; 
  kind        : ('a,'b) kind;
  mutable buf : ('a,'b,c_layout) Bigarray.Array1.t; 
}

open Unix

(** What we need to use mmap; for large file sizes, we need to be on a
   64 bit machine *)
module type S = sig 
  val int_size_is_geq_63: bool
end

module Make_1(S:S) = struct
  open S
  let _ = assert(int_size_is_geq_63 && Sys.int_size >= 63)

  type nonrec ('a,'b) t = ('a,'b) t

  let const_1Bi = 1073741824

  (* double up to 1B, then grow by 1B each time *)
  let next_sz sz = 
    match () with
    | _ when sz >= const_1Bi -> sz + const_1Bi
    | _ when sz < 64 -> 128
    | _ -> sz * 2

  let empty_buffer kind = Bigarray.Array1.create kind c_layout 0

  let map fd kind sz = 
    let shared = true in
    let buf = 
      Unix.map_file fd kind c_layout shared [| sz |] 
      |> array1_of_genarray
    in
    buf

  (* NOTE sz is the size in terms of the element involved; eg ints each take 8 bytes *)
  let remap sz t = 
    Printf.printf "Resizing %d\n%!" sz;
    (* release old buffer *)
    t.buf <- empty_buffer t.kind;
    (* force collection of old buffer; perhaps try to detect
       finalization *)
    Gc.full_major ();
    (* create new map *)    
    let buf = map t.fd t.kind sz in
    t.buf <- buf 
    

  let bytes_per_elt = Bigarray.kind_size_in_bytes 


  (* NOTE public functions from here *)
  
  (* FIXME shared is always true? if we are unmapping and remapping,
     we presumably want the new data to be changed *)
  let of_fd fd kind = 
    let sz = (Unix.fstat fd).st_size in
    (* NOTE we don't increase the size at this point; that can happen
       later *)
    let buf = map fd kind (sz/bytes_per_elt kind) in
    { fd; kind; buf }

  let length t = Array1.dim t.buf 

  let rec sub t ~off ~len = 
    let buf_len = Array1.dim t.buf in
    match off+len <= buf_len with
    | true -> Array1.sub t.buf off len
    | false -> 
      remap (next_sz (off+len)) t;
      sub t ~off ~len

  let msync t = Msync.msync (genarray_of_array1 t.buf)
      
  (* let fstat t = Unix.fstat t.fd *)

  let close t = 
    msync t;
    t.buf <- empty_buffer t.kind;
    Gc.full_major ();
    Unix.close t.fd
end


module Make_2(S:S) : Mmap_intf.S = Make_1(S)


module Export = struct
  include Make_2(struct 
      let int_size_is_geq_63 = (Sys.int_size >= 63)
    end)

  type char_mmap = (char,Bigarray.int8_unsigned_elt)t

  type int_mmap = (int,Bigarray.int_elt)t    

end

