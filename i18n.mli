(** This is half-baked module to provide I18N in simple Eliom app
All resources are XML files, which are stored in base_dir/lang_code/resource_name.xml
Any module can have its own resources - if they use functor Locale

I18n has predefined resource for translation of string within Eliom services - 
it's accesible via GetText module - with interface compatible with gettex.

Before using I18n it has to be initialized with init function.

*)

val set_base_dir : string -> unit
val set_default_lang : string -> unit
val get_available_languages : unit -> (string * bytes) list
val set_lang : string -> unit
val set_lang_from_header: unit -> unit
val get_current_lang : unit -> string
val fmt_calendar : CalendarLib.Calendar.t -> string
val get_resource_from_file : string -> string -> string Lwt.t
val get_resource_from_file_blocking : string -> string -> bytes
val iter_all_resources_blocking : string -> (string -> bytes -> unit) -> unit
exception ResourceNotAvailable
module type Localized =
  sig
    type entry
    type key
    val expected_size : int
    val resource_file : string
    val parse_resource : string -> (key * entry) list
  end
module type S =
  sig
    type entry
    type key
    type lang = string
    module Blocking :
      sig
        val load_resource : lang -> unit
        val get_res : lang -> key -> entry
        val get_resource : key -> entry
        val load_all_languages : unit -> unit
      end
    val get_resource : key -> entry Lwt.t
    val get_res : lang -> key -> entry Lwt.t
    val load_resource : lang -> unit Lwt.t
  end
module Locale :
  functor (L : Localized) ->  (S with type entry=L.entry and type  key=L.key)
   
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
    val load_resources : unit -> unit
  end
val init : ?lang:string -> ?base:string -> unit -> unit
