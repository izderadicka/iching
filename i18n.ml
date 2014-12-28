open Lwt

let base_dir = ref "locale"
let default_lang = ref "en"
let set_base_dir d =
  base_dir:=d
let set_default_lang l =
  default_lang := l
  
let language:string option Eliom_reference.Volatile.eref = Eliom_reference.Volatile.eref 
						    ~scope:Eliom_common.default_session_scope  None

let set_lang s =
  Eliom_reference.Volatile.set language (Some s)

let get_current_lang ()  = 
try 
  match Eliom_reference.Volatile.get language with
    | Some l -> l
    | None -> !default_lang 
with
  Eliom_common.Eliom_site_information_not_available _ -> !default_lang
 
let cal_fmt = ref "%Y-%m-%d %H:%M %z"
let fmt_calendar c =
  CalendarLib.Printer.Calendar.sprint !cal_fmt c

(* read file according to language version *) 
let get_resource_from_file lang fname = 
 let open Lwt_io in
 with_file ~mode:input (Filename.concat (Filename.concat !base_dir lang) fname)
	   (fun inch ->
	    read inch
	   )


let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  s

let get_resource_from_file_blocking lang fname =
 load_file  (Filename.concat (Filename.concat !base_dir lang) fname)


let list_langs_available_blocking () =
  let dirs = Array.to_list (Sys.readdir !base_dir) in
  let langs= List.filter (fun n ->
	       (String.length n) >= 2 &&
		 (String.length n) <= 3 &&
		   n.[0] != '.' &&
		     (Sys.is_directory (Filename.concat !base_dir n)) &&
		       (Sys.file_exists (Filename.concat (Filename.concat !base_dir n) "lang"))
		       
	      ) dirs in
  List.map (fun l ->
	    l, (load_file  (Filename.concat (Filename.concat !base_dir l) "lang"))
	   ) langs

let get_all_resources_blocking fname process = 
let langs = list_langs_available_blocking () in
List.iter (fun (lang, _) ->
	   let s = get_resource_from_file_blocking lang fname in
	   process lang s
	  ) langs

exception ResourceNotAvailable

module type Localized =
sig
type entry
type key

val  expected_size:int
val resource_file:string 
val parse_resource: string -> (key * entry) list
end

module type S =
sig
type entry
type key
type lang = string

module Blocking: sig
    val load_resource: lang -> unit
    val get_res:lang -> key -> entry
    val get_resource: key -> entry
    val load_all_languages: unit -> unit
end

val get_resource: key -> entry Lwt.t
val get_res:lang -> key -> entry Lwt.t
val load_resource: lang -> unit Lwt.t

end

module Locale (L:Localized): (S with type entry=L.entry and type  key=L.key)  = 
struct
type entry = L.entry
type key = L.key
type lang = string

let expected_size = L.expected_size
let resource_file = L.resource_file

let loaded_lang = Hashtbl.create 5 
let catalog = Hashtbl.create expected_size

let update_catalog lang s = 
  List.iter (fun (k,v) -> Hashtbl.add catalog (lang,k) v) (L.parse_resource s);
  Hashtbl.add loaded_lang lang true

module Blocking = struct
    let load_resource lang = 
      let s = get_resource_from_file_blocking lang resource_file in
      update_catalog lang s

    let load_all_languages () = 
       Hashtbl.clear catalog;
       Hashtbl.clear loaded_lang;
      get_all_resources_blocking resource_file 
				 (fun lang s -> update_catalog lang s)

    let rec get_res lang key =
      if Hashtbl.mem catalog (lang, key) then 
	(Hashtbl.find catalog (lang,key))
      else 
	( if lang = !default_lang then raise ResourceNotAvailable 
	  else get_res !default_lang key
	)

    let get_resource key = 
      let lang = get_current_lang () in
      get_res lang key
end


let load_resource lang =
  get_resource_from_file lang resource_file >|= update_catalog lang
   

let  get_res lang key =
if Hashtbl.mem loaded_lang lang then  
  return (Blocking.get_res lang key)
else 
( load_resource lang >>=
    fun () ->
   return  (Blocking.get_res lang key)
)
 
let get_resource key = 
  let lang = get_current_lang () in
  get_res lang key
end
  
exception InvalidTextFile of string

type lang_term = Singular_string of string|Singular_format of string|Plural_string of string*string
	    |Plural_format of string*string 

(*dummy module to match gettext interface for now *)
module Gettext :
sig
  val s_ : string -> string
  val f_ :
    ('a, 'b, 'c, 'c, 'c, 'd) format6 -> ('a, 'b, 'c, 'c, 'c, 'd) format6
  val sn_ : string -> string -> int -> string
  val fn_ :
    ('a, 'b, 'c, 'c, 'c, 'd) format6 ->
    ('a, 'b, 'c, 'c, 'c, 'd) format6 ->
    int -> ('a, 'b, 'c, 'c, 'c, 'd) format6
  val load_resources: unit -> unit
end
=
struct

module T = Locale(struct

type entry = lang_term
type key = lang_term

let expected_size = 100
let resource_file = "text.xml"

open Simplexmlparser
open Xml_util

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

let parse_xml = function
  | [Element ("text", [], terms)] -> List.map parse_term terms
  | _ -> raise (InvalidTextFile "Invalid root element")

let  parse_resource  s =
  parse_xml (xmlparser_string s)

end)

let load_resources  = 
  T.Blocking.load_all_languages

let get_res_opt r =
  try 
    Some (T.Blocking.get_resource  r)
  with
  | ResourceNotAvailable ->
     None

let s_ s = 
  match get_res_opt (Singular_string s) with
  | Some (Singular_string r) -> r
  | None -> "!!" ^ s
  | _ -> assert false
let f_ fmt = 
  let fmt_text = string_of_format fmt in
  match get_res_opt (Singular_format fmt_text) with
  | Some (Singular_format r) -> Scanf.format_from_string r fmt
  | None -> "!!" ^^ (format_of_string fmt)							 
  | _ -> assert false
let sn_ ss sp n = 
  match get_res_opt (Plural_string (ss,sp)) with
  | Some (Plural_string (ss,sp)) -> if n <=1 then  ss else sp
  | None -> if n <=1 then  "!!" ^ ss else "!!" ^ sp
  | _ -> assert false

let fn_ fs fp n = 
 let ss_text, sp_text = string_of_format fs, string_of_format fp in
 match get_res_opt (Plural_format (ss_text,sp_text)) with
 | Some (Plural_format (rs,rp)) -> if n <=1 then 
			     (Scanf.format_from_string  rs fs) 
			   else 
			     (Scanf.format_from_string rp fp)
 | None ->  if n <=1 then 
			    "!!" ^^ (format_of_string fs) 
			   else 
			    "!!" ^^ (format_of_string fp)
 | _ -> assert false
		
end

let init ?(lang="en") ?(base="locale") () =
  default_lang:= lang;
  base_dir:=base;
  Gettext.load_resources ()
