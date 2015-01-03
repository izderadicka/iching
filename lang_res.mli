type lang_term =
    Singular_string of string
  | Singular_format of string
  | Plural_string of string * string
  | Plural_format of string * string
exception InvalidTextFile of string
val parse_xml : string -> (lang_term * lang_term) list
val output_term : (string -> 'a) -> lang_term -> lang_term -> 'a
