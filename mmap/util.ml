

(** Common type abbrevs *)
module Type_abbrevs = struct

  type char_bigarray = (char,Bigarray.int8_unsigned_elt,Bigarray.c_layout)Bigarray.Array1.t

  type int_bigarray = (int,Bigarray.int_elt,Bigarray.c_layout)Bigarray.Array1.t

  (** The kind of the mmap'ed array; see Bigarray.kind *)
  type ('a,'b) kind = ('a,'b) Bigarray.kind

  let char_kind : (char,Bigarray.int8_unsigned_elt) kind = Bigarray.Char

  let int_kind : (int,Bigarray.int_elt) kind = Bigarray.Int

end


