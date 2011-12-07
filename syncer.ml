type command = 
  | Help
  | Signature
  | Test

let calculate_signature fn sig_fn = 
  let bs = (*... calculate this ... *) 512 in
  let s = Signature.create fn bs in
  let oc = open_out sig_fn in
  Signature.output_signature oc s;
  close_out oc


let _ =
  let fn = ref "" in
  let signature = ref "" in
  let command = ref Help in
  let usage = "___usage___" in
  let speclist =  [
    ("--signature", 
     Arg.Tuple [
       Arg.Unit (fun () -> command := Signature);
       Arg.Set_string fn;
       Arg.Set_string signature;
     ], "<input> <sig> : generate a signature file");
    ("--test",
     Arg.Unit (fun () -> command := Test), "run tests");
  ] 
  in
  let () = Arg.parse
    speclist
    (fun s -> fn := s)
    usage
  in
  match !command with 
    | Help -> Arg.usage speclist usage
    | Signature -> calculate_signature !fn !signature
    | Test -> Test.suite ()

