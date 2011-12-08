module type WEAK = sig
  type t
  val make : unit -> t
  val from : string -> int -> int -> t
  val reset: t -> unit
  val digest: t -> int
  val rotate: t -> char -> char -> unit
  val update: t -> string -> int -> int -> unit
end

module type STRONG = sig
  type t
  val to_hex : t -> string
  val file : string -> t
  val substring: string -> int -> int -> t
  val write : out_channel -> t -> unit
  val read : in_channel -> t
end

module SDigest = (struct
  include Digest
  let read ic = Io.read_string ic
  let write oc t = Io.write_string oc t

end : STRONG)
