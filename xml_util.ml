open Simplexmlparser

exception NonTextElement of string

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
  | e  -> raise (NonTextElement (Printf.sprintf "Element %s does not match:\n %s" name (simple_print e )))
