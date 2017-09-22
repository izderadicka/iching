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

let output_term' w t1 t2 =
  if not(Set.mem !printed t1) then
    Lang_res.output_term w t1 t2;
  printed:= (Set.add !printed t1)

let output_term w  t  idx s  = 
  let ss i =
    get_substring s i
  in
  let o s = 
    if t then "" else s
  in
  match idx with
  | 0 -> output_term' w (Singular_string (ss 1)) (Singular_string (o(ss 1)) )
  | 1 -> output_term' w (Singular_format (ss 1)) (Singular_format(o(ss 1)))
  | 2 -> output_term' w (Plural_string  (ss 1,ss 2)) (Plural_string  (o(ss 1),o(ss 2)))
  | 3 -> output_term' w (Plural_format (ss 1,ss 2)) (Plural_format (o(ss 1),o(ss 2)))
  | _ -> assert false

let with_opt_file file f =
  let out = match file with
    | Some f -> Out_channel.create f
    |None -> Out_channel.stdout
in
let w = Out_channel.output_string out in
(try
  f w
with e -> ( Out_channel.close out; raise e));
Out_channel.close out


let extract files out templ () =
with_opt_file out (fun w ->
		   w "<text>\n";
		   List.iter ~f:(fun f -> 
				 let txt = In_channel.read_all f in
				 List.iteri res ~f: 
					    (fun idx rex ->
					     try 
					       let ml = exec_all ~rex txt in
					       Array.iter ml ~f: (output_term w templ idx)
					     with
					     |Not_found -> ()
					    )
					    
				) files;
		   w "</text>"
		  )

let merge f1 f2 out del () =
  with_opt_file out (fun w ->
		     let s1 = Lang_res.parse_xml (In_channel.read_all f1) in
		     let s2 = Lang_res.parse_xml (In_channel.read_all f2) in
		     let c1 = List.fold s1 ~init:(Set.Poly.empty) ~f:(fun s (k,_) -> Set.add s k) in 
		     let c2 = List.fold s2 ~init:(Set.Poly.empty) ~f:(fun s (k,_) -> Set.add s k) in  
		     let o1 = if del then List.filter s1 ~f:(fun (k,_) -> Set.mem c2 k) else s1 in
		     let o2 = List.filter s2 ~f:(fun (k,_) -> not (Set.mem c1 k)) in
		     w "<text>\n";
		     List.iter o1 ~f:(fun (s1,s2) -> Lang_res.output_term w s1 s2);
		     if not (List.is_empty o2) then
			    begin
			      w (sprintf "<!-- Added on %s -->\n" (Date.to_string_iso8601_basic 
								     (Date.today ())));
			      List.iter o2 ~f:(fun (s1,s2) -> Lang_res.output_term w s1 s2)
			    end;
		     w "</text>"
		    )
											       

let extract_cmd = Command.basic
	    ~summary: "extracts strings for localication"
	    Command.Spec.(empty +>
			     anon (sequence ("files" %: file)) +>
			    flag "-o" (optional file) ~doc: "Output file" +>
			    flag "-t" no_arg ~doc:"template output - translation not filled"
			  )
			    
	    extract
let merge_cmd = Command.basic
		  ~summary: "merge two files together"
		  Command.Spec.( empty +>
				    anon ("file1" %: file) +>
				    anon  ("file2" %: file) +>
				    flag "-o" (optional file) ~doc: "Output file" +>
				    flag "-d" no_arg ~doc: "Delete entries in file1 which are not present in file2"
				)
				  
		  merge

let cmd = Command.group
	    ~summary: "tool to support localization"
	    [("extract", extract_cmd);
	    ("merge", merge_cmd)]
let () =
  Command.run ~version:"1.0"
	      cmd


