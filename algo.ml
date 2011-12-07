
module Weak = Adler
module Strong = Digest

type action = 
  | Match of int * int
  | Miss  of Buffer.t
  | Start of int
  | Stop 

let output_action oc = function
  | Match (b,e) -> 
    Io.write_byte oc 1;
    Io.write_int oc b;
    Io.write_int oc e
  | Miss b ->
    Io.write_byte oc 2;
    Io.write_string oc (Buffer.contents b)
  | Start s ->
    Io.write_byte oc 3;
    Io.write_int oc s
  | Stop  -> Io.write_byte oc 4
      
let input_action ic = 
  match input_byte ic with
    | 1 -> 
      let b = Io.read_int ic in
      let e = Io.read_int ic in
      Match (b,e)
    | 2 -> 
      let len = Io.read_int ic in
      let b = Buffer.create len in
      let () = Buffer.add_channel b ic len in
      Miss b
    | 3 -> 
      let s = Io.read_int ic in
      Start s
    | 4 -> Stop
    | _ -> failwith "unknown action"

open Signature


class delta_emitter signature new_fn handler = 
  let bs = signature.bs in
  let buffer_size = 8 * bs in
  let buffer = String.create buffer_size in
object(self)
  val mutable _read = 0
  val mutable _first_free = 0
  val mutable _n_free = buffer_size
  val mutable _first_todo = 0 
  val mutable _previous_action = Start bs
  val mutable _finished = false
  val mutable _weak_ok = false
  val _weak = Weak.make ()
    
  method _examine_block buffer offset length = 
    let wd = Weak.digest _weak in
    match lookup_weak signature wd with
      | None -> None
      | Some block ->
	let strong = Strong.substring buffer offset length in
	if strong = block.strong 
	then Some block.index
	else None

  method _miss char =
    match _previous_action with
      | Miss b when Buffer.length b < bs ->  Buffer.add_char b char
      | other ->
	let () = handler _previous_action in
	let b = Buffer.create bs in
	let () = Buffer.add_char b char in
	_previous_action <- Miss b

  method _match index = 
    match _previous_action with
      | Match (b,e)  when e + 1 = index -> _previous_action <- Match(b,index)
      | other -> let () = handler _previous_action in
		 _previous_action <- Match(index,index)

  method run () = 
    let ic = open_in new_fn in
    while not _finished do
      begin
	let read = input ic buffer _first_free _n_free in
	if read = 0 then
	  _finished <- true
	else
	  let () = _first_free <- _first_free + read in
	  let () = _n_free <- _n_free - read in
	  ()
      end;
      while _first_todo + bs < _first_free do
	if not _weak_ok then 
	  begin
	    Weak.update _weak buffer _first_todo bs;
	    _weak_ok <- true
	  end;
	begin
	  match self # _examine_block buffer _first_todo bs with
	    | None -> 
	      self # _miss buffer.[_first_todo];
	      Weak.rotate _weak buffer.[_first_todo] buffer.[_first_todo + bs];
	      _first_todo <- _first_todo + 1
	    | Some i -> 
	      self # _match i;
	      _first_todo <- _first_todo + bs;
	      _weak_ok <- false;
	      Weak.reset _weak
	end
      done;
      if _first_todo + bs >= _first_free 
      then
	begin
	  let length = _first_free - _first_todo in
	  String.blit buffer _first_todo buffer 0 length;
	  _first_todo <- 0;
	  _first_free <- length;
	  _n_free <- buffer_size - length
	end
    done;
    let rec loop i = 
      if i = _first_free 
      then ()
      else 
	let () = self # _miss (buffer.[i]) in
	loop (i+1) 
    in
    loop _first_todo;
    handler _previous_action;
    handler Stop;
    close_in ic
end


class delta_applier ic (old_fn:string) oc = 
  let fd = Unix.openfile old_fn [Unix.O_RDONLY] 0o640 in
  let really_read buffer bs = 
    let rec loop pos todo = 
      if todo = 0 then ()
      else
	let read = Unix.read fd buffer pos todo in
	loop (pos+ read) (todo - read)
    in
    loop 0 bs
  in
object(self)
  method run () = 
    let bs = match input_action ic with
      | Start s -> s
      | _ -> failwith "start @ beginning"
    in
    let rec loop () = 
      let action = input_action ic in
      match action with
	| Match(b,e) -> self # apply_match bs b e ; loop ()
	| Miss b -> self # apply_miss b ; loop ()
	| Start s -> failwith "can't restart"
	| Stop -> ()
    in
    loop ();
    Unix.close fd

  method apply_match bs b e = 
    let buffer = String.create bs in
    let n = e + 1 - b in
    let rec loop i = 
      if i = 0 then ()
      else
	let () = really_read buffer bs in
	let () = output_string oc buffer in
	loop (i-1)
    in
    let _ = Unix.lseek fd (bs * b) Unix.SEEK_SET in
    loop n
      
  method apply_miss b = output_string oc (Buffer.contents b)
end
