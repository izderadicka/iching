open Core.Std
open Pcre
open Lang_res

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



let res = [s_re;f_re;sn_re;fn_re]

let printed = ref Set.Poly.empty 

let output_term' w f t1 t2 =
  if not(Set.mem !printed t1) then
    Lang_res.output_term w f t1 t2;
  printed:= (Set.add !printed t1)

let output_term w  f t  idx s  = 
  let ss i =
    get_substring s i
  in
  let o s = 
    if t then "" else s
  in
  match idx with
  | 0 -> output_term' w f (Singular_string (ss 1)) (Singular_string (o(ss 1)) )
  | 1 -> output_term' w f (Singular_format (ss 1)) (Singular_format(o(ss 1)))
  | 2 -> output_term' w f (Plural_string  (ss 1,ss 2)) (Plural_string  (o(ss 1),o(ss 2)))
  | 3 -> output_term' w f (Plural_format (ss 1,ss 2)) (Plural_format (o(ss 1),o(ss 2)))
  | _ -> assert false


let extract files out templ () =
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
			     Array.iter ml ~f: (output_term w f templ idx)
			   with
			   |Not_found -> ()
			  )
				    
	      ) files;
 w "</text>";
 Out_channel.close out

let extract_cmd = Command.basic
	    ~summary: "extracts strings for localication"
	    Command.Spec.(empty +>
			     anon (sequence ("files" %: file)) +>
			    flag "-o" (optional file) ~doc: "Output file" +>
			    flag "-t" no_arg ~doc:"template output - translation not filled"
			  )
			    
	    extract

let cmd = Command.group
	    ~summary: "tool to support localization"
	    [("extract", extract_cmd)]
let () =
  Command.run ~version:"1.0"
	      cmd


