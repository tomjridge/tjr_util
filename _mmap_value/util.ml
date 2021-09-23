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


(** Write to mmap using a bin_writer; allocate len space initially; if
   not enough space, increase the buffer size and try again. *)
let write_increasing ~(bin_write_v:_ Bin_prot.Write.writer) ~len ~mmap ~off ~v =
  len |> iter_k (fun ~k len -> 
      try
        bin_write_v (Mmap.sub mmap ~off ~len) ~pos:0 v (* returns length of written value *)
      with Bin_prot.Common.Buffer_short -> k (len*2))

let read_increasing ~(bin_read_v:_ Bin_prot.Read.reader) ~len ~mmap ~off =
  len |> iter_k (fun ~k len -> 
      try
        bin_read_v (Mmap.sub mmap ~off ~len) ~pos_ref:(ref 0)
      with Bin_prot.Common.Buffer_short -> k (len*2))
      
  
