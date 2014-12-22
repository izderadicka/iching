open Simplexmlparser
open I18n
open Lwt

module L = struct
type t = {no:int;change:string;predication:string;comment:string}
end


module J = struct
type t = {predication:string;comment:string}
end

type hexagram={no:int;name:string;translated:string;comment:string;
	       judgement:J.t;image:J.t;lines:L.t list}

exception InvalidHexagramFile of string

let rec print_attrs  = function
  | (n,v)::t -> " "^n^"=\""^v^"\""^(print_attrs t)
  | [] -> ""

let rec simple_print = function
  | Element (n, attrs, children) ->
     "<"^n^(print_attrs attrs)^">\n"^(List.fold_left (fun s e -> s ^ (simple_print e)) "" children)^"</"^n^">\n"
  | PCData s ->"[PCDATA["^s^"]]"

let  join_lines lines = 
String.concat "\n" 
(List.map (function 
	    |PCData s -> s
	    |_ -> assert false)

(List.filter (function 
	       |PCData _ -> true
	       |_ -> false)
	     lines
))


let text_from_el name = function
  | Element (n,_ ,lines) when n=name -> join_lines lines
  | e  -> raise (InvalidHexagramFile (Printf.sprintf "Element %s does not match:\n %s" name (simple_print e )))

let judgement_from_el name = function
  | Element (n, [], [predication;comment]) when name = n ->
		       {J.predication = text_from_el "predication" predication;
			J.comment = text_from_el "comment" comment}
  | _ -> raise (InvalidHexagramFile (Printf.sprintf "Element %s has incorrect structure" name ))

let lines_from_el  = function 
  | Element ("lines", [], lines) ->
     let line_from_el = function
       | Element ("line", ["no", no], [change;predication;comment])->
	  {L.no = int_of_string no;
	   L.change = text_from_el "change" change;
	   L.predication= text_from_el "predication" predication;
	   L.comment =text_from_el "comment" comment
	  }
       | _ -> raise (InvalidHexagramFile "Element line has incorrect structure")
     in
     List.map (fun e -> line_from_el e) lines
  | _ -> raise  (InvalidHexagramFile "Element lines has incorrect structure")
	    

let hexagram_from_xml  = function 
  | Element ("hexagram", ["no",no], [name;translated;comment;judgement;image;lines]) ->
     {no=int_of_string no;
      name= text_from_el "name"  name;
      translated= text_from_el "translated" translated;
      comment= text_from_el "comment" comment;
      judgement = judgement_from_el "judgement" judgement;
      image = judgement_from_el "image" image;
      lines = lines_from_el  lines
     }
  | _ -> raise  (InvalidHexagramFile "Element hexagram has incorrect structure")

let parse_xml s =
    let xml = xmlparser_string s in
    match xml with 
    | [Element ("hexagrams", [], hexs)] ->
       List.map (fun h -> hexagram_from_xml h) hexs
    | _ -> raise  (InvalidHexagramFile "Element hexagrams has incorrect structure")


module Loc= Locale(struct
		   type entry = hexagram
		   type key = int

		   let  expected_size= 64*5
		   let resource_file="hexagrams.xml"
		   let parse_resource s =
		     List.map (fun t -> t.no,t) (parse_xml s)
		 end)

let get_hexagram_text (no:int) =
  Loc.get_resource no

let get_hexa_comment (no:int)
 =
 Loc.get_resource no >|=
   fun ht -> ht.comment

let get_hexa_judgement_comment (no:int) =
  Loc.get_resource no >|=
   fun ht -> ht.judgement.J.comment

let get_hexa_image_comment (no:int) =
  Loc.get_resource no >|=
   fun ht -> ht.image.J.comment

let get_hexa_line_comment (no:int) (line:int) =
   Loc.get_resource no >|=
   fun ht -> let l= List.find (fun l -> l.L.no = line) ht.lines in
	     l.L.comment


let get_lines no selected =
let open Lwt in
get_hexagram_text no >|=
fun hexa -> 
let lines =hexa.lines in
let moving = List.filter (fun l -> List.mem l.L.no selected) lines in
if (List.length lines) = 6 then 
try 
  [List.find (fun l -> l.L.no =0) lines]
with
|Not_found -> moving
else moving
