
open Simplexmlparser
open Xml_util

type lang_term = Singular_string of string|Singular_format of string|Plural_string of string*string
	    |Plural_format of string*string 

exception InvalidTextFile of string

let parse_term = function
  | Element ("term", ("type", "s")::_ , [txt;sng]) -> Singular_string (text_from_el "text" txt), 
						     Singular_string (text_from_el "trans" sng)
  | Element ("term", ("type", "f")::_, [txt;sng]) -> Singular_format (text_from_el "text" txt), 
						     Singular_format (text_from_el "trans" sng)
  |  Element ("term", ("type", "sn")::_, [txt_sng;txt_pl;sng;pl]) ->
      Plural_string (text_from_el "sng" txt_sng, text_from_el "pl" txt_pl),
      Plural_string (text_from_el "sng_trans" sng, text_from_el "pl_trans" pl)
  |  Element ("term", ("type", "fn")::_, [txt_sng;txt_pl;sng;pl]) ->
      Plural_format (text_from_el "sng" txt_sng, text_from_el "pl" txt_pl),
      Plural_format (text_from_el "sng_trans" sng, text_from_el "pl_trans" pl)
  | e -> raise (InvalidTextFile (Printf.sprintf "Invalid term %s" (simple_print e)))

let parse_xml s =
match xmlparser_string s with 
  | [Element ("text", [], terms)] -> List.map parse_term terms
  | _ -> raise (InvalidTextFile "Invalid root element")

let encode_term = function
  | Singular_string s -> Singular_string (Xml_print.encode_unsafe_char s)
  |Singular_format f -> Singular_format (Xml_print.encode_unsafe_char f)
  |Plural_string (s1,s2) -> Plural_format (Xml_print.encode_unsafe_char s1, Xml_print.encode_unsafe_char s2)
  |Plural_format (f1,f2) -> Plural_format (Xml_print.encode_unsafe_char f1, Xml_print.encode_unsafe_char f2)

let output_term w t1 t2 =
let open Printf in
match ((encode_term t1),(encode_term t2)) with
    | Singular_string s1, Singular_string s2 -> 
       w (sprintf "<term type=\"s\">\n<text>%s</text>\n<trans>%s</trans>\n</term>\n" s1 s2)
    | Singular_format s1, Singular_format s2 -> 
       w (sprintf "<term type=\"f\">\n<text>%s</text>\n<trans>%s</trans>\n</term>\n" 
		     s1 s2)
    | Plural_string (s1,s2), Plural_string (s1',s2') -> 
       w (sprintf "<term type=\"sn\">\n<sng>%s</sng>\n<pl>%s</pl>\n<sng_trans>%s</sng_trans>\n<pl_trans>%s</pl_trans>/n</term>\n" 
		    s1 s2 s1' s2')
    | Plural_format (s1,s2), Plural_format (s1',s2') ->  w (sprintf "<term type=\"fn\">\n<sng>%s</sng>\n<pl>%s</pl>\n<sng_trans>%s</sng_trans>\n<pl_trans>%s</pl_trans>/n</term>\n"
		    s1 s2 s1' s2');
    | _ -> assert false

