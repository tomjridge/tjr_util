(** This exposes file IO via pread/pwrite, and read/write via a
   user-supplied "pos ref"; as such, this interface is thread safe,
   and convenient for reading sequentially as well as randomly. *)

(* FIXME what about signals, EINTR etc; extunix has pread and pwrite
   continuing on intr, so should be fine *)

module Private = struct

  module Unixfile = struct
    open struct module Ext = ExtUnix.Specific end

    let fsync = Ext.fsync
    let fdatasync = Ext.fdatasync
    let sync = Ext.sync
    let syncfs = Ext.syncfs
    let dirfd = Ext.dirfd

    (* FIXME maybe include *at calls here *)

    let realpath = Ext.realpath 


    (** NOTE if an exception occurs, we do not know how much of the
       data was actually written. *)
    let pread_all ~fd ~off ~buf ~buf_off ~len = 
      Ext.pread fd off buf buf_off len |> fun n_read -> 
      (* n_read may be less than len in the case that there was not enough data
         in the file *)
      n_read

    let pwrite_all ~fd ~off ~buf ~buf_off ~len = 
      Ext.pwrite fd off (Bytes.unsafe_to_string buf) buf_off len |> fun n_writ -> 
      assert(n_writ = len);
      n_writ

    let read_all ~fd ~(pos:int ref) ~buf ~buf_off ~len =
      pread_all ~fd ~off:(!pos) ~buf ~buf_off ~len |> fun n_read -> 
      pos:=!pos + n_read;
      n_read

    let write_all ~fd ~(pos:int ref) ~buf ~buf_off ~len =
      pwrite_all ~fd ~off:(!pos) ~buf ~buf_off ~len |> fun n_writ -> 
      pos:=!pos + n_writ;
      n_writ

    let close_noerr fd = try Unix.close fd with _e -> ()

    let fstat_size fd = Unix.((fstat fd).st_size)
        
  end

  module type S = sig
    val fsync : Unix.file_descr -> unit
    val fdatasync : Unix.file_descr -> unit
    val sync : unit -> unit
    val syncfs : Unix.file_descr -> unit
    val dirfd : Unix.dir_handle -> Unix.file_descr
    val realpath : string -> string
    val pread_all :
      fd:Unix.file_descr ->
      off:int -> buf:bytes -> buf_off:int -> len:int -> int
    val pwrite_all :
      fd:Unix.file_descr ->
      off:int -> buf:bytes -> buf_off:int -> len:int -> int

    (** NOTE the pos ref is updated at the end of the call. *)
    val read_all :
      fd:Unix.file_descr ->
      pos:int ref -> buf:bytes -> buf_off:int -> len:int -> int

    (** NOTE the pos ref is updated at the end of the call. *)
    val write_all :
      fd:Unix.file_descr ->
      pos:int ref -> buf:bytes -> buf_off:int -> len:int -> int

    val close_noerr : Unix.file_descr -> unit

    val fstat_size : Unix.file_descr -> int

  end

end

include (Private.Unixfile : Private.S)
