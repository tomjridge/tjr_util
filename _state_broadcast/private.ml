(** Private implementation *)

open Bin_prot.Std
open Intf
open Util

module Make(S:S) = struct


  module Common = struct

    module Ptr = struct
      let header_start = 0
      let header_len = 1024
      let data_start = 1024
    end

    module Header = struct
      type t = {
        mutable last_state_offset:int;
        mutable delta_len:int
      }[@@deriving bin_io]
    end

    let write_header mmap (t:Header.t) = 
      Mmap.sub mmap ~off:0 ~len:Ptr.header_len |> fun ba -> 
      ignore(Header.bin_write_t ba ~pos:0 t);
      ()

    let read_header mmap =
      Mmap.sub mmap ~off:0 ~len:Ptr.header_len |> fun ba -> 
      Header.bin_read_t ba ~pos_ref:(ref 0)

    
    let write_x ~bin_write_x ~mmap ~off ~x ~len_ref = 
      (* If the buf is not large enough, we double the size until it
         is; start with size 1024 *)
      assert(!len_ref > 0);
      !len_ref |> iter_k (fun ~k:kont len -> 
          try 
            let ba = Mmap.sub mmap ~off ~len in
            ignore(bin_write_x ba ~pos:0 x);
            len_ref := max len !len_ref;
            ()
          with Bin_prot.Common.Buffer_short -> 
            kont (2*len))

    let read_x ~bin_read_x ~mmap ~off ~len_ref =
      assert(!len_ref > 0);
      !len_ref |> iter_k (fun ~k:kont len -> 
          try 
            let ba = Mmap.sub mmap ~off ~len in
            let r = ref 0 in
            let x = bin_read_x ba ~pos_ref:r in
            len_ref := max !r !len_ref;
            x
          with Bin_prot.Common.Buffer_short -> 
            kont (2*len))

    let write_state ~mmap ~off ~state ~len_ref =
      write_x ~bin_write_x:S.bin_write_state ~mmap ~off ~x:state ~len_ref

    let read_state ~mmap ~off ~len_ref = 
      read_x ~bin_read_x:S.bin_read_state ~mmap ~off ~len_ref

    let write_delta ~mmap ~off ~state ~len_ref =
      write_x ~bin_write_x:S.bin_write_delta ~mmap ~off ~x:state ~len_ref

    let read_delta ~mmap ~off ~len_ref = 
      read_x ~bin_read_x:S.bin_read_delta ~mmap ~off ~len_ref    
      
  end

  open Common

  module Writer = struct

    include S

    type t = { 
      fd                        : Unix.file_descr; 
      mmap                      : (char,Bigarray.int8_unsigned_elt) Mmap.t; 
      mutable last_state_offset : int; (* -1 for "not valid" *)
      mutable delta_len         : int; (* number of deltas following last state *)
      state_size: int ref; (* max state size in bytes, for marshalling *)
      delta_size: int ref;
    }






  end

end
