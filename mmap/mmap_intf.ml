(** Main mmap types *)

open Util.Type_abbrevs


(** To write to/from the mmap, use the sub function to get the
   subarray of interest. NOTE that because the map may grow when
   asking for a sub which extends beyond the current length, a remap
   may occur. Thus, subs should not be reused or stored, since they
   may refer to an out of date slice for an old mapping. For this
   reason, subs are not thread safe (a new thread may unmap a sub
   being used by an older thread). *)
module type S = sig
  type ('a,'b) t

  (** Number of bytes used to store each elt *)
  val bytes_per_elt : ('a,'b) kind -> int

  val of_fd  : Unix.file_descr -> ('a,'b) kind -> ('a,'b) t
  
  (** Length of the mmap in terms of number of elts; should match the
     fstat size (taking bytes_per_elt into account) *)
  val length : _ t -> int

  (** Subarray from offset of the given length *)
  val sub : ('a,'b) t -> off:int -> len:int -> ('a,'b,Bigarray.c_layout) Bigarray.Array1.t

  (** Perform an msync to flush data to disk *)
  val msync  : _ t -> unit

  val close  : _ t -> unit
end


