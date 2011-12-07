
let write_byte = output_byte

let write_int oc i = Marshal.to_channel oc i []
let read_int  ic   = Marshal.from_channel ic

let read_string ic = 
  let ss = read_int ic in
  let s = String.create ss in
  let () = really_input ic s 0 ss in
  s
    
let write_string oc s = 
  write_int oc (String.length s);
  output_string oc s
    
