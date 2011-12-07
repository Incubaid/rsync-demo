let fn = "/tmp/evil.bin"

let v = 3910028790 

let write () = 
  let oc = open_out fn in
  output_binary_int oc v;
  close_out oc

let read () = 
  let ic = open_in fn in
  let v2 = input_binary_int ic in
  let () = close_in ic in
  v2


let () = 
  let () = write () in
  let v2 = read () in
  Printf.printf "%08x %08x\n" v v2
