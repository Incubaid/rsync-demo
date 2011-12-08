open Hash
open Unix

module Signature = functor (W:WEAK) -> functor (S:STRONG) -> struct
  type block_signature = {weak:int; strong:S.t; index:int}
      
  let compare_weak ba bb = compare ba.weak bb.weak 
  let compare_index ba bb = compare ba.index  bb.index
  let bs_strong bs = bs.strong
  let bs_index bs = bs.index

  type t = {len:int; bs: int; blocks: block_signature array;}

  let block_size t = t.bs
  let length t = Array.length t.blocks

  let optimal fn = 
    let stat = Unix.stat fn in
    let size = stat.st_size in
    let rc = int_of_float (2. *. sqrt (float size)) in (* thumb *)
    let r = if rc < 32 then 32 else rc in
    r
    
  let lookup_weak t w = 
    let rec find min max =
      let mid = (min + max) / 2 in
      let block = t.blocks.(mid) in
      let weak = block.weak in
      if w = weak then Some block 
      else if min > max then None
      else if w > weak then find (mid+1) max
      else find min (mid -1)
    in 
    let len = length t in
    find 0 (len -1)
      
  let create fn bs = 
    let ic = open_in fn in
    let len = in_channel_length ic in
    let buf = String.create bs in
    let read_block size index = 
      let () = really_input ic buf 0 size in
      let a = W.from buf 0 size in
      let weak = W.digest a in
      let strong = S.substring buf 0 size in
      {weak;strong;index}
    in
    let rec read_blocks acc todo i = 
      if todo >= bs then
        let block = read_block bs i in
        read_blocks (block :: acc) (todo - bs) (i+1)
      else
        let block = read_block todo i in
        List.rev (block :: acc)
    in
    let blocks_l = read_blocks [] len 0 in
    let blocks = Array.of_list blocks_l in
    let () = close_in ic in
    Array.sort compare_weak blocks;
    {len;bs;blocks;}
      
  let output_signature oc t = 
    Io.write_int oc t.len;
    Io.write_int oc t.bs;
    Io.write_int oc (length t);
    let i = ref 0 in
    let one block = 
      Io.write_int oc block.weak;
      S.write oc block.strong;
      assert (block.index = !i);
    (* i is skipped: in order because sorted *)
      incr i
    in
    Array.sort compare_index t.blocks;
    Array.iter one t.blocks;
    Array.sort compare_weak t.blocks
      
  let input_signature ic = 
    let len     = Io.read_int ic in
    let bs      = Io.read_int ic in
    let nblocks = Io.read_int ic in
    let rec loop acc index = 
      if index = nblocks then List.rev acc 
      else
        let weak = Io.read_int ic in
        let strong = S.read ic in
        let b = {weak;strong;index} in 
        loop (b :: acc) (index + 1) in
    
    let blocks_l = loop [] 0 in
    let blocks = Array.of_list blocks_l in
    let r =   {len;bs;blocks} in
    Array.sort compare_weak r.blocks;
    r
      
    
  let to_file t fn =
    let oc = open_out fn in
    let () = output_signature oc t in
    close_out oc 
      
  let from_file fn = 
    let ic = open_in fn in
    let s = input_signature ic in
    close_in ic;
    s
      

  let equals t1 t2 = 
    let so_far = 
      t1.len = t2.len &&
      t1.bs  = t2.bs in
    let size1 = Array.length t1.blocks in
    let size2 = Array.length t2.blocks in
    let rec loop i acc = 
      if i = size1 then acc
      else 
        begin
	  let bs1 = t1.blocks.(i) 
	  and bs2 = t2.blocks.(i) in
	  let acc' = acc && bs1 = bs2 
	  and i' = i+1 in
	  loop i' acc'
        end
    in
    let r = so_far && size1 = size2 && loop 0 true in
    r
      
end
