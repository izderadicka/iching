open Lwt

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
end
=
struct
let s_ s =s
let f_ fmt = format_of_string fmt
let sn_ ss sp n = if n <=1 then  ss else sp
let fn_ fs fp n = if n <=1 then (format_of_string fs) else (format_of_string fp)
end

let base_dir = ref "/home/ivan/workspace/iching/text"
let default_lang = ref "en"
let set_base_dir d =
  base_dir := d
let set_default_lang l =
  default_lang := l
  
let language:string option Eliom_reference.Volatile.eref = Eliom_reference.Volatile.eref 
						    ~scope:Eliom_common.default_session_scope  None

let set_lang s =
  Eliom_reference.Volatile.set language s

let get_current_lang ()  = 
  match Eliom_reference.Volatile.get language with
    | Some l -> l
    | None -> !default_lang 
 
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

let get_resource_from_file_blocking lang fname =
let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  (s)
in
load_file  (Filename.concat (Filename.concat !base_dir lang) fname)

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

val get_resource: key -> entry Lwt.t
val get_res:lang -> key -> entry Lwt.t
val get_res':lang -> key -> entry
val load_resource: lang -> unit Lwt.t
val load_resource_blocking: lang -> unit

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
  List.iter (fun (k,v) -> Hashtbl.add catalog (lang,k) v) (L.parse_resource s)

let load_resource lang =
  get_resource_from_file lang resource_file >|= update_catalog lang
   

let load_resource_blocking lang = 
  let s = get_resource_from_file_blocking lang resource_file in
  update_catalog lang s
  

let rec get_res' lang key =
  if Hashtbl.mem catalog (lang, key) then 
     (Hashtbl.find catalog (lang,key))
  else 
    ( if lang = !default_lang then raise ResourceNotAvailable 
      else get_res' !default_lang key
    )

let rec get_res lang key =
if Hashtbl.mem loaded_lang lang then  
  return (get_res' lang key)
else 
( load_resource lang >>=
    fun () ->
    Hashtbl.add loaded_lang lang true;
    get_res lang key
)
 
let get_resource key = 
  let lang = get_current_lang () in
  get_res lang key
end
  
