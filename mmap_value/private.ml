(** Private implementation *)

module type S = sig
  type v[@@deriving bin_io]
end  


let const_9 = 9 (* bin_prot takes 9 bytes to store int64 *)

module Non_atomic = struct
  type t = {
    fd: Unix.file_descr;
    mmap: Mmap.char_mmap;
    len_buf : Mmap.char_bigarray; (* where we write the length *)
    mutable generation : int;
    mutable len : int; (* max length of marshalled value *)
  }

  module Ptr = struct
    let len = 0
    let data = const_9
  end

  (* This version backed by a single file; updates are not atomic; not
     concurrent safe *)
  module Make_1(S:S) = struct
    open S

    let write' ~mmap ~v ~len =
      Util.write_increasing ~bin_write_v ~len ~mmap ~off:Ptr.data ~v

    let write_len ~len_buf ~len = 
      ignore(Bin_prot.Write.bin_write_int len_buf ~pos:0 len)

    let read_len ~len_buf =
      Bin_prot.Read.bin_read_int len_buf ~pos_ref:(ref 0)

    (** Create, with initial value v, expected to fit in buffer of size len *)
    let create ~fn ~(init:v) = 
      let len = 1024 in
      let fd = Unix.(openfile fn [O_CREAT;O_TRUNC;O_RDWR] 0o640) in
      let mmap = Mmap.(of_fd fd char_kind) in
      let len_buf = Mmap.sub mmap ~off:0 ~len:Ptr.data in
      let generation = 1 in
      write' ~mmap ~v:init ~len |> fun len ->
      write_len ~len_buf ~len;
      { fd;mmap;len_buf;generation;len }

    let write t v = 
      write' ~mmap:t.mmap ~v ~len:t.len |> fun len -> 
      t.len <- max len t.len;
      write_len ~len_buf:t.len_buf ~len

    let read t =
      let len = read_len ~len_buf:t.len_buf in
      Util.read_increasing ~bin_read_v ~mmap:t.mmap ~off:Ptr.data ~len

    let close t = 
      Mmap.close t.mmap

  end
end

type config = {
  ctl:string;
  fn0:string;
  fn1:string;
}

type t = {
  ctl_mmap : Mmap.int_mmap;
  ctl_buf  : Mmap.int_bigarray;  
  files    : Non_atomic.t Array.t; (* indexed by 0,1 *)
  mutable current : int;
  mutable generation : int;
}

(** This version maintains a control file, and two mmaped values with
   a current pointer to one of them; updates go to the non-current,
   and the current pointer is then updated. *)
module Make_2(S:S) = struct
  (* open S *)

  type nonrec t = t
  type v = S.v

  module Ptr = struct
    let current = 0
    let generation = 1
    let len = 2 (* ie current and generation *)
  end

  module Non_atomic_ = Non_atomic.Make_1(S)
  
  let write_ctl ctl_buf ~current ~generation = 
    ctl_buf.{Ptr.current} <- current;
    ctl_buf.{Ptr.generation} <- generation;
    ()

  let create ~config ~init = 
    let {ctl;fn0;fn1} = config in
    let ctl_fd = Unix.(openfile ctl [O_CREAT;O_TRUNC;O_RDWR] 0o640) in
    let ctl_mmap = Mmap.(of_fd ctl_fd int_kind) in
    let ctl_buf = Mmap.sub ctl_mmap ~off:0 ~len:Ptr.len in (* current and generation *)
    let fns = [| fn0; fn1 |] in
    let files = fns |> Array.map (fun fn -> Non_atomic_.create ~fn ~init) in
    let current = 0 in
    let generation = 1 in
    write_ctl ctl_buf ~current ~generation;
    { ctl_mmap; ctl_buf; files; current; generation }

  let write_value t v =
    (* index for file we write to *)
    let i = 1 - t.current in
    Non_atomic_.write t.files.(i) v;
    t.current <- t.current + 1;
    t.generation <- t.generation + 1;
    write_ctl t.ctl_buf ~current:t.current ~generation:t.generation;
    t.generation

  (* FIXME check concurrency here; FIXME make sure can't loop
     indefinitely *)
  let rec read_value t =
    let g = t.generation in
    let i = t.current in
    let v' = try Some (Non_atomic_.read t.files.(i)) with _ -> None in
    match v' with
    | Some v -> (
      let g' = t.generation in
      match g=g' with
      | true -> v,g
      | false -> read_value t)
    | None -> read_value t

  let generation t = t.generation
                   
  let close t = 
    Mmap.close t.ctl_mmap;
    t.files |> Array.iter (fun f -> Non_atomic_.close f);
    ()
end

module type S2 = sig
  include Intf.S

  val create : config:config -> init:v -> t
end

module Make_3(S:S) : S2 with type v = S.v = Make_2(S)

