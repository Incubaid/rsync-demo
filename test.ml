open Algo
open Signature

let old_dir = "tests/old/"
let new_dir = "tests/new/" 
let sync_dir = "tests/synced/"

let test_io () = 
  let values = [128;127;4;1;0;] in
  let do_one v = 
    let fn = "/tmp/test_io.bin" in
    let oc = open_out fn in
    let () = Io.write_int oc v in
    let () = close_out oc in
    let ic = open_in fn in
    let v2 = Io.read_int ic in
    let () = close_in ic in
    Printf.printf "%i <> %i\n" v v2;
    assert (v = v2)
  in
  List.iter do_one values

let test_signature fn bs =
  Printf.printf "SIGNATURE:%s\n" fn;
  let old_fn = old_dir ^ fn in
  let signature = Signature.create old_fn bs in
  let signature_fn = sync_dir ^ fn ^ ".signature" in
  let () = Signature.to_file signature signature_fn in
  let signature2 = Signature.from_file signature_fn in
  Printf.printf "%s: %b \n%!" fn (Signature.equals signature2 signature)

let test_one fn bs = 
  let old_fn = old_dir ^ fn in
  let signature = Signature.create old_fn bs in
  let signature_fn = sync_dir ^ fn ^ ".signature" in
  let delta_fn = sync_dir ^ fn ^ ".delta" in
  let () = Signature.to_file signature signature_fn in
  let oc = open_out delta_fn in
  let handle_action action = output_action oc action in
  let new_fn = new_dir ^ fn in
  let signature2 = Signature.from_file signature_fn in 
  (* let signature2 = signature in *)
  assert (signature2 = signature);
  let emitter = new delta_emitter signature2 new_fn handle_action in
  let () = emitter # run () in
  let () = close_out oc in
  let ic = open_in delta_fn in
  let sync_fn = sync_dir ^ fn in
  let sync_oc = open_out sync_fn in
  let applier = new delta_applier ic old_fn sync_oc in
  applier # run () ;
  close_out sync_oc;
  let d_new    = Digest.to_hex(Digest.file new_fn) in
  let d_synced = Digest.to_hex(Digest.file sync_fn) in
  Printf.printf "%s: %s =?= %s\n%!" fn d_new d_synced;
  ()

let test_correctness () = 
  let tests = ["small.txt", 16;
	       "fischer.txt",16;
	       "big.bmp", 256; 
	      ]
  in
  List.iter (fun (fn,bs) -> test_signature fn bs) tests;
  List.iter (fun (fn,bs) -> test_one fn bs) tests ;;

let suite () = 
  test_io ();
  test_correctness();;
