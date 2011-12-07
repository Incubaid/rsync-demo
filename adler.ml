type t = {mutable a: int; mutable b: int; mutable c: int}

let padler = 65521
  
let make () = {a = 1 ;b = 0; c = 0}
  
let from buf offset length = 

  let too_far = offset + length in
  let rec loop a b i= 
    if i = too_far 
    then {a; b; c = length}
    else (* slooow modulo can be lifted with a bit of math *)
      let a' = (a + Char.code(buf.[i])) mod padler in
      let b' = (b + a') mod padler in
      loop a' b' (i+1)
  in
  loop 1 0 offset
    
let reset t = t.a <- 1;t.b <- 0; t.c <- 0
  
let digest t = (t.b lsl 16) lor t.a
  
let rotate t c1 cn = 
  let x1 = Char.code c1 in
  let xn = Char.code cn in
  t.a <- (t.a - x1 + xn) mod padler;
  t.b <- (t.b - t.c * x1 + t.a -1) mod padler
    
let update t buf offset length = 
  let too_far = offset + length in 
  let rec loop i = 
    if i = too_far then () 
    else
      let x = Char.code buf.[i] in
      let () = t.a <- (t.a + x) mod padler in
      let () = t.b <- (t.b + t.a) mod padler in
      let () = t.c <- t.c + 1 in
      loop (i +1)
  in
  loop offset

