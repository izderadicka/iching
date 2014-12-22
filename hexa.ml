let fuxi_order=
[[02;23;08;20;16;35;45;12];
 [15;52;39;53;62;56;31;33];
 [07;04;29;59;40;64;47;06];
 [46;18;48;57;32;50;28;44];
 [24;27;03;42;51;21;17;25];
 [36;22;63;37;55;30;49;13];
 [19;41;60;61;54;38;58;10];
 [11;26;05;09;34;14;43;01]
]



let hexa_map = 
  let a = Array.make 64 0 in
  let _ = List.fold_left ( fun row_index row ->
			   let _ = List.fold_left (fun col_index cell ->
					   a.(cell-1) <- col_index + 8 * row_index; col_index + 1 )
					  0 row 
			   in
			   row_index+1
			 )
	 
			 0 fuxi_order
  in a

let hexa_rev_map =
 let a = Array.make 64 0 in
  let _ = List.fold_left ( fun row_index row ->
			   let _ =  List.fold_left (fun col_index cell ->
					   a.( col_index + 8 * row_index) <-cell; col_index + 1 )
					  0 row 
			   in
			   row_index+1
			 )
	 
			 0 fuxi_order
  in a

let tri_map = [|5;4;3;6;2;7;8;1|]

type line = Full of bool | Broken of bool

type hexagram = line list

type trigram = line list


let hexa_from_index i = 
let num = hexa_map.(i-1) in
let rec decode n bits res =
  if bits>0 then 
    let l = if n land  1 > 0 then Full false
	    else Broken false
    in decode ( n lsr 1) (bits-1) (l::res)
  else res
in decode num 6 []

let rec encode hexa n =
    match hexa with
    | (Full _) :: tl -> encode tl ((n lsl 1) lor 1)
    | (Broken _) :: tl -> encode tl (n lsl 1)
    | [] -> n

let index_from_hexa h =
  let n = encode h 0 in
  assert ((n>=0) && (n<64));
  hexa_rev_map.(n)

let rec tri_from_hex' = function
  | [a;b;c;d;e;f] -> (encode [a;b;c] 0), (encode [d;e;f] 0)
  | _ -> assert false

let tri_from_hex h =
let a,b = tri_from_hex' h in
tri_map.(a),tri_map.(b) 
  

let rec hexa_from_numbers = function
  | [] -> []
  | n::tl -> 
     let line = match n with
       | 6 -> Broken true
       | 7 -> Full false
       | 8 -> Broken false
       | 9 -> Full true
       | _ -> assert false
     in line::hexa_from_numbers tl

let rec numbers_from_hexa  = function
  |h::t -> let num = match h with
	     | Broken true -> 6
	     | Broken false -> 8
	     | Full true -> 9
	     | Full false -> 7
	   in
	   num::numbers_from_hexa t
  |[] -> []
   

let complementary_hexa h =
  let rec compl h exists =
    match h with
    | o::t -> let nl,e = 
		( match o with
		  |Full true -> (Broken false), true
		  |Broken true -> (Full false), true
		  | x -> x, false
		) in
	      let r,e =compl t ( e || exists) in
	      (nl::r), e		       
    | [] -> ([],exists)
  in
  compl h false
    
let  moving_lines h =
let rec cl count  = function
  | Broken true :: t | Full true :: t -> count::(cl (count+1) t)
  | _ :: t -> cl (count+1) t
  | [] -> []
in
cl 1 h
