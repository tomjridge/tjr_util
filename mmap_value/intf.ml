module type S = sig

  type t

  type v

  val read_value : t -> v * int

  (** Returns the generation for the new value *)
  val write_value : t -> v -> int

  val generation : t -> int  

  val close : t -> unit

end
