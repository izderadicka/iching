let buf_to_int b =
  let bytes_needed = Buffer.length b in
  assert (bytes_needed mod 4 = 0); 
  let rec buf_to_int pos i =
    if pos>= bytes_needed then i
    else
      begin
	let s = Buffer.sub b pos 4 in
	let rec to_num n i =
	  if n<=3 then to_num (n+1) ((i lsl 8) lor (int_of_char s.[n]))
	  else i
	in
	let n = to_num 0 0 in
	buf_to_int (pos+4) (i lxor n)
      end
  in
  buf_to_int 0 0

let list_to_string ~printer res =
  List.fold_left (fun r b ->
		  let sep = match r with
		    | "" -> ""
		    | _ -> ","
		  in
		  r ^ sep  ^ (printer  b)) "" res

let string_to_list ~reader s =
let l = Str.split (Str.regexp "\\s*,\\s*") s in
List.map reader l

let rec range ?(start=0) stop =
if start < stop then start :: (range ~start:(start+1)) stop else []

let memoize ?(size=1024) f =
let t = Hashtbl.create size in
(fun x -> 
 try
   Hashtbl.find t x
with
| Not_found -> 
   let y = f x in
   Hashtbl.add t x y;
   y
)


