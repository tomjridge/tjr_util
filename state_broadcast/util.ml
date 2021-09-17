(** Essentially the Y combinator; useful for anonymous recursive
   functions. The k argument is the recursive callExample:

{[
  iter_k (fun ~k n -> 
      if n = 0 then 1 else n * k (n-1))

]}


 *)
let iter_k f (x:'a) =
  let rec k x = f ~k x in
  k x

(* trace execution *)
let trace (s:unit->string) = 
  (* print_endline (s()) *)
  ignore(s); ()
[@@warning "-27"]

let warn (s:unit->string) = 
  print_endline (s())
