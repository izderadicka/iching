type trigram = {
  no : int;
  name : string;
  translated : string;
  comment : string option;
}
exception InvalidTrigramsFile

val get_trigram_text : int -> trigram  Lwt.t
val parse_xml: string -> trigram list
