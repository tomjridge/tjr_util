(** Interfaces *)

(* State and delta type *)
module type S = sig
  type state[@@deriving bin_io]
  type delta[@@deriving bin_io]
end

module type WRITER = sig

  include S

  type t

  (* NOTE no initial state required; but broadcast state must be called first *)
  val create : fn:string -> t

  val broadcast_delta : t -> delta -> unit

  val broadcast_state : t -> state -> unit

end

module type READER = sig

  include S

  type t

  val open_ : fn:string -> apply_delta:(state -> delta -> state) -> t

  val read_state : unit -> state

end
