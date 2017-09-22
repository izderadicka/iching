open Simplexmlparser
open I18n

type trigram = {no:int;name:string;translated:string;comment:string option}

exception InvalidTrigramsFile

let trigram_from_xml = function
  | Element ("trigram", ["no",no], children) ->
     begin
       match children with
       | Element ("name",[], [PCData name])::
	   Element ("translated", [], [PCData translated])::
	     t -> let comment = match t with
		    | [Element ("comment", [], [PCData c])] -> Some c
		    | [] -> None
		    | _ -> raise InvalidTrigramsFile
		  in 
		  {no=(int_of_string no);name;translated;comment}
       |_ -> raise InvalidTrigramsFile
     end
  |_ -> raise InvalidTrigramsFile

let parse_xml s = 
let xml = xmlparser_string s in
match xml with
| [Element ("trigrams", [], ts)] -> List.map (fun t -> trigram_from_xml t) ts
| _ -> raise InvalidTrigramsFile

module Loc= Locale(struct
		   type entry = trigram
		   type key = int

		   let  expected_size= 8*5
		   let resource_file="trigrams.xml"
		   let parse_resource s =
		     List.map (fun t -> t.no,t) (parse_xml s)
		 end)

let get_trigram_text no =
  Loc.get_resource no
