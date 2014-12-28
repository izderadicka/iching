module L :
  sig
    type t = {
      no : int;
      change : string;
      predication : string;
      comment : string;
    }
  end
module J : sig type t = { predication : string; comment : string; } end
type hexagram = {
  no : int;
  name : string;
  translated : string;
  comment : string;
  judgement : J.t;
  image : J.t;
  lines : L.t list;
}

exception InvalidHexagramFile of string

val parse_xml: string -> hexagram list

val get_hexagram_text : int -> hexagram Lwt.t
val get_hexa_comment : int -> string Lwt.t
val get_hexa_judgement_comment : int -> string Lwt.t
val get_hexa_image_comment : int -> string Lwt.t
val get_hexa_line_comment : int -> int -> string Lwt.t
val get_lines : int -> int list -> L.t list Lwt.t
