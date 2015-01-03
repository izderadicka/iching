open Eliom_content
open Html5
open Lwt

let is_touch_device () = 
  let res  = Js.Unsafe.fun_call (Js.Unsafe.variable "isTouchDevice") [| |] in
  Js.to_bool res

let rnd_throws cb  = 
  let bytes_needed = if is_touch_device () then  16 else 64 in
  let bytes_needed_f = float_of_int bytes_needed in
  let b =Buffer.create bytes_needed in 
  let count= ref 0 in
  let byte = ref 0 in
  let throw = ref 0 in
  let progress_sig, progress_update = React.S.create 0.0 in
  let gen (x,y) =
    let rnd = (x land 3) lxor ( y land 3) lxor 
		(int_of_float(1_000_000. *. Sys.time ()) land 3) in
    byte := (!byte lsl 2) lor rnd;
    incr count;
    if !count >3 then ( 
      count:= 0; 
      Buffer.add_char b (char_of_int !byte ); 
      byte:=0;
      let p = 100. *. (float_of_int (Buffer.length b)) /. bytes_needed_f in
      progress_update p 
    );
    if Buffer.length b >= bytes_needed then
      (
	progress_update 100.0;
	let seed = Util.buf_to_int b in
	Random.init(seed);
	let throws =[ Random.bool ();  Random.bool ();  Random.bool ()] in
	cb !throw throws; 
	incr throw;
	Buffer.clear b;
	progress_update 0.0;
      )
    
  in gen, progress_sig

let rec shake_coins coins =
  let size = 10 in
  let gen_rnd () = 
    Printf.sprintf "%dpx" ((size/2) - (Random.int size)) 
  in
  match coins with
  | c :: tl -> Manip.SetCss.top c (gen_rnd ()); Manip.SetCss.left c (gen_rnd ()); shake_coins tl
  | [] -> ()
  

let launch_coins  main_elt progress_bar remaining coins throws_area next_btn = 
  let area = Html5.To_dom.of_div main_elt in
  let remaining_sig, update_remaining = React.S.create 6 in
  let on_throw no res = 
    	Eliom_lib.debug "Thrown coins - attempt %d - result:  %s" no 
			(Util.list_to_string ~printer:string_of_bool res);
	update_remaining ((React.S.value remaining_sig) - 1);

	if no <= 5 then 	(* prevent action from buffred events *)
	  begin
	    let value = Common.throw_value res in
	    Manip.appendChild throws_area 
			      Html5.F.(li ([span ~a:[a_class ["throw-no"]] [pcdata (string_of_int (no+1))]] @
					     Common.mk_three_coins ~small:true res @ 
					       [span ~a:[a_class ["throw-result"]] 
						     [pcdata " = "; 
						      pcdata (string_of_int value)]
					  ])
					  
	    );
	    (* below is equivalent to JS code document.getElementById("hexa_"+no).value = value *)
	    let input = Js.coerce_opt (Dom_html.document##getElementById(Js.string(Printf.sprintf "hexa_%d" no)))
				      Dom_html.CoerceTo.input (fun _ -> assert false) in
	    input##value <- (Js.string (string_of_int value))
	  end;
	if no>=5 then
	  (
	    Manip.removeSelf main_elt;
	    Manip.removeSelf progress_bar;
	    Manip.SetCss.display next_btn "block"
	  )
  in
  let gen, progress_sig = rnd_throws on_throw in
  let throw_coins (x,y) =
     shake_coins coins;
     gen (x,y)
  in
  Manip.appendChild progress_bar 
		    (D.div 
		       ~a:[F.a_class ["progress-bar"];
			   R.a_style (React.S.map (fun v -> Printf.sprintf "width: %0.2f%%;" v) progress_sig)
			  ]
		       []
		    );
  Manip.appendChild remaining (R.pcdata (React.S.map string_of_int remaining_sig));
  let _mouse_thread = Lwt_js_events.mousemoves
			area
			(fun evt _ ->
			  let x,y = evt##clientX, evt##clientY in
			  return (throw_coins (x,y)) 
			) 
  in
  let _touch_thread = Lwt_js_events.touchmoves 
			area
			(fun evt _ ->
			 let l =  evt##changedTouches in
			 let n =  l##length in
			 let rec proc_touches i =
			 if i < n then
			   (let evt = Js.Optdef.get  l##item(i)  (fun () -> failwith "touch evt undefined") in
			    let x,y = evt##clientX, evt##clientY in
			   gen  (x,y);
			   proc_touches (i+1)
			   ) 
			 in
			 shake_coins coins;
			 proc_touches 0;
			 Dom.preventDefault evt;
			 return ()
			)
  in
  ()
			  
let launch_counter jin jang jin_signal jang_signal =
Manip.appendChild jin (R.pcdata (React.S.map string_of_int jin_signal));
Manip.appendChild jang (R.pcdata (React.S.map string_of_int jang_signal))
				       
let create_dialog () =
let open F in
let running =  div ~a:[a_class ["progress"]] [] in
let dia = D.div ~a:[a_class ["dialog"]] [running] in
ignore (Manip.addEventListener dia Dom_html.Event.click (fun _ _->  Manip.removeSelf dia; false));
Dom.appendChild (Dom_html.document##body) (To_dom.of_div dia);
return dia

let update_dialog dia s close_msg top =
  let open F in
  Manip.removeChildren dia;
  Manip.appendChild dia (div ~a:[a_class ["dialog-content"];
				 a_style (Printf.sprintf "top:%dpx;" top);
				 a_onclick (fun e -> Dom_html.stopPropagation e) ] 
  [span ~a:[a_class ["dialog-text"]] [pcdata s];
   div ~a:[a_class ["next-btn"];
	    a_onclick (fun _ -> Manip.removeSelf dia) ] 
	 [pcdata close_msg]]);
  return ()

let show_info_handler get_comment close_msg =
fun e -> 
let elt = Js.Opt.get (e##currentTarget) (fun () -> failwith "No target in click event?") in
let top = (elt##offsetTop) + (elt##offsetHeight) in
async (
    fun () ->
    create_dialog () >>=
      fun dia ->
      get_comment () >>=
      fun info -> 	    
      update_dialog dia info close_msg top
  )



     
let add_hexa_svg cont h =
  let c = To_dom.of_div cont in
  let svg = To_dom.of_node (Common.hexa_svg h) in
  ignore ( c##appendChild(svg) );
  Eliom_lib.debug "Added svg"
  
