open Eliom_content.Html5
open D


let mk_coin ?(state=false) ?(small=false) () =  
 let cls = if small then ["coin";"small"] else ["coin"] in
 let cls =if state then   "obverse"::cls else "reverse"::cls in
 div ~a:[a_class cls] []

let mk_three_coins ?(small=false)  states =
  assert ((List.length states) = 3);
  List.map (fun state -> mk_coin ~state ~small  ()) states


let throw_value states =
  let nums = List.map (fun b -> if b then 3 else 2) states in
  List.fold_left (fun acc v -> acc+v) 0 nums

let  a_hexa = Custom_data.create ~name:"hexa" 
				~to_string: (Util.list_to_string ~printer:string_of_int)
				~of_string: (Util.string_to_list ~reader:int_of_string)
				()

let hexa_svg h = 
  let open Eliom_content.Svg.F in
  let x_offset, y_offset = 5. , 13. in
  let stroke_black = "stroke:rgb(0,0,0);stroke-width:8"  in
  let stroke_red = "stroke:rgb(255,0,0);stroke-width:2"  in
  let r = 5.0 in
  let ln (x1,y1,x2,y2) style =
    let x1,y1,x2,y2 = (x1, Some `Px),  (y1, Some `Px),  (x2, Some `Px),  (y2, Some `Px) in
    line ~a: [a_x1 x1;a_y1 y1; a_x2 x2 ;a_y2 y2;a_style style] [] 
  in
  let crc (x,y) r  style =
    let x,y,r =  (x, Some `Px),  (y, Some `Px),  (r, Some `Px) in
    circle ~a: [a_cx  x; a_cy y; a_r  r ; a_style style] []
  in
  let rec draw_line y  = function
    | [] -> []
    | l :: tl  -> 
       
       let elems=  match l with 
	   | Hexa.Full m -> 
	      let l= ln (x_offset,y,(x_offset +. 80.), y) stroke_black  in
	      if m then 
		[l;crc ((x_offset +. 40.), y)  r  stroke_red]
	      else
		[l]
		     
				 
	 | Hexa.Broken m  ->  
	    let l =    [ln (x_offset, y, (x_offset +. 30.),  y) stroke_black;
			ln ((x_offset +. 50.), y, (x_offset +. 80.),  y) stroke_black
		       ] in
	    if m then 
	      l @ [ ln ((x_offset +. 40. -. r),  (y +. r), (x_offset +. 40. +. r),  (y -. r)) stroke_red;
		ln ((x_offset +. 40. -. r),  (y -. r), (x_offset +. 40. +. r),  (y +. r)) stroke_red
	      ]
	    else
	      l
			 
       in
       elems @ (draw_line (y +. y_offset) tl)
  in  svg ~a:[a_width (90., Some `Px) ;a_height (83., Some `Px)]  (draw_line 9. (List.rev h))



