open Core.Std
open Pcre

let regular_file =
  Command.Spec.Arg_type.create
    (fun filename ->
       match Sys.is_file filename with
       | `Yes -> filename
       | `No | `Unknown ->
         eprintf "'%s' is not a regular file.\n%!" filename;
         exit 1
    )

let s_re = regexp "\\(\\s*s_\\s*\"([^\"]*)\"\\s*\\)"
let f_re = regexp "\\(\\s*f_\\s*\"([^\"]*)\"\\s*\\)"
let sn_re = regexp "\\(\\s*s_\\s*\"([^\"]*)\"\\s*\"([^\"]*)\"\\s*(\\d+)\\s*\\)"
let fn_re = regexp "\\(\\s*f_\\s*\"([^\"]*)\"\\s*\"([^\"]*)\"\\s*(\\d+)\\s*\\)"

type expr = Singular_string of string|Singular_format of string|Plural_string of string*string
	    |Plural_format of string*string 

let res = [s_re;f_re;sn_re;fn_re]

let printed = ref Set.Poly.empty 
let output_term' w f t =
  if not(Set.mem !printed t) then
    match t with
    | Singular_string s1 -> w (sprintf "<term type=\"s\" file=\"%s\">\n<text>%s</text>\n<trans>%s</trans>\n</term>\n" 
		    f s1 s1)
    | Singular_format s1-> w (sprintf "<term type=\"f\" file=\"%s\">\n<text>%s</text>\n<trans>%s</trans>\n</term>\n" 
		    f s1 s1)
    | Plural_string (s1,s2) -> w (sprintf "<term type=\"sn\" file=\"%s\">\n<sng>%s</sng>\n<pl>%s<\pl>\n<sng_trans>%s</sng_trans>\n<pl_trans>%s</pl_trans>/n</term>\n" 
		    f s1 s2 s1 s2)
    | Plural_format (s1,s2) ->  w (sprintf "<term type=\"fn\" file=\"%s\">\n<sng>%s</sng>\n<pl>%s<\pl>\n<sng_trans>%s</sng_trans>\n<pl_trans>%s</pl_trans>/n</term>\n"
		    f s1 s2 s1 s2);
printed:= (Set.add !printed t)

let output_term w  f idx s  = 
  let ss i =
    Xml_print.encode_unsafe_char (get_substring s i)
  in
  match idx with
  | 0 -> output_term' w f (Singular_string (ss 1))
  | 1 -> output_term' w f (Singular_format (ss 1))
  | 2 -> output_term' w f (Plural_string  (ss 1,ss 2))
  | 3 -> output_term' w f (Plural_format (ss 1,ss 2))
  | _ -> assert false


let extract files out () =
let out = match out with
  | Some f -> Out_channel.create f
  |None -> Out_channel.stdout
in
let w = Out_channel.output_string out in
w "<text>\n";
 List.iter ~f:(fun f -> 
	       let txt = In_channel.read_all f in
	       List.iteri res ~f: 
			  (fun idx rex ->
			   try 
			     let ml = exec_all ~rex txt in
			     Array.iter ml ~f: (output_term w f idx)
			   with
			   |Not_found -> ()
			  )
				    
	      ) files;
 w "</text>";
 Out_channel.close out

let cmd = Command.basic
	    ~summary: "Extracts strings for localication"
	    Command.Spec.(empty +>
			     anon (sequence ("files" %: file)) +>
			    flag "-o" (optional file) ~doc: "Output file"
			  )
			    
	    extract


let () =
  Command.run ~version:"1.0"
	      cmd


