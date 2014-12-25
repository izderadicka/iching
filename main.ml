open Eliom_content
open Html5
open Common
open I18n.Gettext
open Lwt

(* Configuration of the app *)

let read_config () =
  let open Simplexmlparser in
  let  proc = function
    | Element ("locale", [("dir",d)], []) -> I18n.set_base_dir d; Eliom_lib.debug "Set locale base dir to %s" d
    | _ -> Eliom_lib.debug "Unknown config elem"
  in
  let cfg = Eliom_config.get_config () in
  Eliom_lib.debug "Procesing config - %d elems" (List.length cfg);
  List.iter proc cfg

let () = read_config ()

(******************************************************************************************************)
module Iching_app =
  Eliom_registration.App (
    struct
      let application_name = "iching"
    end)
(* Session state and persistance*)
(******************************************************************************************************)

let question = Eliom_reference.eref ~scope:Eliom_common.default_session_scope  None
let hexagram = Eliom_reference.eref ~scope:Eliom_common.default_session_scope  []
let result_id = Eliom_reference.eref ~scope:Eliom_common.default_session_scope  None 
let results = Ocsipersist.open_table "iching_results"

let get_current_oracle () =
  Eliom_reference.get question >>=
    fun q -> Eliom_reference.get hexagram >|=
	       fun h -> 
	       match (q,h) with
	       | (Some q, [a;b;c;d;e;f]) -> (q,h,CalendarLib.Calendar.now () )
	       | _ -> raise Eliom_common.Eliom_Wrong_parameter

let set_question q = 
  Eliom_reference.set question q >>=
    fun () -> Eliom_reference.unset result_id

let set_hexagram h =
   Eliom_reference.set hexagram h >>=
    fun () -> Eliom_reference.unset result_id

let get_stored_oracle id =
  Ocsipersist.find results id 

let save_oracle o =
 let id = Uuidm.(to_string (create `V4)) in
  Ocsipersist.add results id  o  >|=
    fun () -> id

let forget_oracle () = 
  Eliom_reference.set question None >>=
  fun () -> Eliom_reference.set hexagram []

(* Services *)
(******************************************************************************************************)
let main_service =
  Eliom_service.App.service ~path:[] ~get_params:Eliom_parameter.unit ()

let coins_service =
  Eliom_service.App.service 
    ~path:["coins"]
    ~get_params: Eliom_parameter.unit
    ()

let question_service =
  Eliom_service.App.service
    ~path:["question"]
    ~get_params: Eliom_parameter.unit
    ()

let save_question_action = 
  Eliom_service.App.post_coservice
    ~fallback: coins_service
    ~post_params: (Eliom_parameter.string "question")
    ~csrf_safe: true
    ()

let result_service = 
 Eliom_service.App.service
   ~path:["result"]
   ~get_params:Eliom_parameter.(suffix(opt(string "saved_result_id")))
   () 

let save_hexa_action = 
  Eliom_service.App.post_coservice
    ~fallback: result_service
    ~post_params: Eliom_parameter.(list "hexa" (int "item"))
    ()

let hexa_picture_service =
  Eliom_service.Http.service
    ~path:["hexagram"; "image"]
    ~get_params: Eliom_parameter.(suffix (int "number"))
    ()
let hexa_picture_service2 =
Eliom_service.Http.service
  ~path:["hexagram"; "image"]
  ~get_params:Eliom_parameter.(suffix (list "hexa" (int "item")))
  ()

let hexa_table_service =
Eliom_service.Http.service
  ~path:["hexagram";"table"]
  ~get_params: Eliom_parameter.unit
  ()

let hexa_info_service =
  Eliom_service.Http.service
    ~path:["hexagram"; "info"]
    ~get_params: Eliom_parameter.(suffix (int "number"))
    ()

let hexa_comment_service =
Eliom_service.Ocaml.coservice'
  ~get_params: (Eliom_parameter.int "hexa_no")
()

let hexa_judgement_comment_service =
Eliom_service.Ocaml.coservice'
  ~get_params: (Eliom_parameter.int "hexa_no")
()

let hexa_image_comment_service =
Eliom_service.Ocaml.coservice'
  ~get_params: (Eliom_parameter.int "hexa_no")
()

let hexa_line_comment_service =
Eliom_service.Ocaml.coservice'
  ~get_params: Eliom_parameter.((int "hexa_no") ** (int "line_no"))
()

let save_result_service =
Eliom_service.App.post_coservice
  ~fallback:result_service
  ~post_params: Eliom_parameter.unit
()


(* Forms,  widgets utils *)
(**********************************************************************************************)

let make_coins_area () =
  let open D in
  let remaining = span ~a:[a_class ["throw-countdown"]] [] in
  let coins = Common.mk_three_coins [false;false;false]  in
  let coins_area = div ~a:[a_class ["coins-area";"centered"]] [
			 F.div ~a:[a_class ["instructions"]] 
			       [pcdata (s_ "Move mouse(finger) around this area to throw coins")];
			 F.div ~a:[a_class ["remaing-msg"]] [remaining; pcdata (s_" throws remain")];
			 F.div ~a:[a_class ["coins"]] coins
		       ] in
  coins_area, remaining, coins

let question_form () =
  D.post_form 
    ~service: save_question_action
    (fun  q_param ->
     let open F in
     [ div ~a:[a_class ["question-form";"centered"]] 
		       [label ~a:[a_for q_param] [pcdata (s_ "Enter your question below:")];
			textarea ~name:q_param ~a:[a_class ["question"]] ()];
       input ~input_type:`Submit ~value:(s_ "Next") ~a:[a_class ["next-btn"]; a_id "question-submit_btn"] ()
     ]) ()
			     
let hexa_form () = 
  D.post_form 
    ~a:[D.a_style "display:none"]
    ~service: save_hexa_action
    (fun hl ->
     let open F in 
     (hl.Eliom_parameter.it (fun name lbl init -> 
			    (int_input ~input_type:`Hidden ~name ~a:[a_id lbl] ()) :: init
			   )
			   (List.map (fun n -> "hexa_" ^ (string_of_int n)) (Util.range 6))
			   []
     ) @ [input ~input_type:`Submit ~a:[a_class ["next-btn"]] ~value:(s_ "Show Oracle") () ]
    )
    None

let make_page ~title ~header body =

let module Html5 = F in
let footer =
(
<:html5list<
<div class="footer centered">
<a href="/"> Interactive I Ching</a> brought to you by <a href="http://zderadicka.eu">Ivan</a>
</div>
>>
)
in
let header_title ,body_cont = header, (body@footer) in
Eliom_tools.F.html ~title 
		   ~css:[["css";"style.css"]]
		   ~js:[["js";"custom.js"]]
		   ~other_head:[F.meta ~a:[F.a_name "viewport"; 
			  F.a_content "width=device-width, initial-scale=1.0 user-scalable=false" ] ()]
		   F.(body
			      ([h1 [pcdata header_title]]@body_cont))


let make_ext_link url txt =
let open Html5.F in
Raw.a ~a:[Raw.a_href (uri_of_string 
			    (fun () -> url ));
	    a_target "_blank"] 
	[pcdata txt]

let break_lines txt =
  let lines= Str.split (Str.regexp "\n") txt in
  let rec join = function
    | l::t -> (F.pcdata l)::(F.br ())::(join t)
    |[] -> []
  in
  join lines

let save_result_form id = 
let open F in
match id with 
|None -> post_form 
	   ~service:save_result_service 
	   (fun () ->
	    [string_input ~input_type:`Submit ~a:[a_class ["next-btn"]] ~value:(s_ "Remember It") ()]
	   )
	   None
|Some _ -> F.div []
  
  
let hexa_picture' h = 
  let open Cairo in
  let pi = 4.0 *. atan 1.0 in
  let img = Image.create Image.ARGB32 ~width:90 ~height:83 in
  let ctx = create img in
  let x_offset, y_offset = 5. , 13. in
  set_line_cap ctx BUTT;
  let stroke_black ctx =
    set_source_rgb ctx 0. 0. 0.;
    set_line_width ctx 8.;
    stroke ctx;
  in
  let stroke_red ctx =
    set_source_rgb ctx 255. 0. 0.;
    set_line_width ctx 2.;
    stroke ctx;
  in
  let r = 5.0 in
  let rec draw_line y  = function
    | [] -> ()
    | l :: tl  -> 
       begin
	 match l with 
	 | Hexa.Full m -> move_to ctx x_offset  y;
		     line_to ctx (x_offset +. 80.)  y;
		     stroke_black ctx;
		     if m then (
		       arc  ctx  (x_offset +. 40.) y r 0.0 (2. *. pi);
		       stroke_red ctx;
		     )
				 
	 | Hexa.Broken m  ->  move_to ctx x_offset  y; 
			 line_to ctx (x_offset +. 30.) y; 
			 move_to ctx (x_offset +. 50.) y;
			 line_to ctx (x_offset +. 80.)  y;
			 stroke_black ctx;
			 if m then (
			   set_source_rgb ctx 255. 0. 0.;
			   set_line_width ctx 2.;
			   move_to ctx  (x_offset +. 40. -. r)  (y +. r);
			   line_to ctx  (x_offset +. 40. +. r)  (y -. r);
			   move_to ctx  (x_offset +. 40. -. r)  (y -. r);
			   line_to ctx  (x_offset +. 40. +. r)  (y +. r);
			   stroke_red ctx;
			 )
       end;
       draw_line (y +. y_offset) tl
  in draw_line 9. (List.rev h);
     let img_bytes () =
       let b = Buffer.create (3 * 70 * 140) in
       PNG.write_to_stream img (Buffer.add_string b);
       Buffer.contents b
     in
     img_bytes ()
     
let hexa_picture = Util.memoize hexa_picture'

(* Registration *)
(**************************************************************************************************)

let () =
  Iching_app.register
    ~service:main_service
    (fun () () ->
      let open Html5.F in 
      return (make_page ~title:(s_ "I Ching - Book of Changes") ~header:((s_ "I Ching")^" (易經) "^(s_"Book of Changes"))
			[div ~a:[a_class ["intro"; "centered"]]
			   [p 
			   [pcdata (s_ "This interactive application presents ") ;
			    make_ext_link "http://en.wikipedia.org/wiki/I_Ching" (s_ "I Ching ");
			    pcdata (s_ " oracle.");
			    br ();
			    pcdata (s_ "Application is written in ");
			    make_ext_link "http://ocaml.org" "Ocaml ";
			    pcdata (s_ " language and web framefork ");
			    make_ext_link "http://ocsigen.org" "Ocsigen";
			    br ();
			    pcdata (s_ "Text is based on Richard Wilhelm transation of the book")
			   ];
			   h2 [pcdata (s_ "How to use:")];
			   ol ~a:[a_class ["instructions"]] [
			     li [pcdata (s_ "Write down a question")];
			     li [pcdata (s_ "Throw coins virually - with help of mouse movements")];
			     li [pcdata (s_ "Read text for resulting "); a ~service:hexa_table_service 
									   ~a:[a_target "_blank"]
									   ~xhr: false
									   [pcdata (s_ "hexagrams")]
									   ()
				] 
			      ];
			];
			 a ~service:question_service ~a:[a_class ["next-btn"]] [pcdata (s_ "Start Here")] ()
			]
	     )
    );
												       
       

  Eliom_registration.String.register
    ~service: hexa_picture_service
    (fun no () ->
     if no >=1 && no <= 64 then
       let p =hexa_picture (Hexa.hexa_from_index no) in
       Lwt.return (p, "image/png")
     else raise Eliom_common.Eliom_Wrong_parameter
    );

  Eliom_registration.String.register 
    ~service:hexa_picture_service2
    (fun h () ->
     if (List.length h) = 6 then
       let p = hexa_picture (Hexa.hexa_from_numbers h) in
       return (p, "image/png")
     else raise Eliom_common.Eliom_Wrong_parameter
    );

  Iching_app.register
    ~service: hexa_table_service
    (fun () () ->
     let open Hexa in
     let open F in
     let t =table ~a:[a_class ["hexa_table"]]
	      (List.fold_right 
	       (fun row r -> (tr (List.fold_right 
				      (fun id r-> 
				       (td [a ~service:hexa_info_service
					    [div ~a:[a_class ["hexa_id"]] [pcdata (string_of_int id)];
					   img ~src:(make_uri ~service:hexa_picture_service id) 
					       ~alt:"hexagram" () ]
					    id]

				       ) :: r )
				      row []
				 )
			     ) :: r
	       )
	       fuxi_order []
	      )
     in
     Lwt.return (make_page ~title: (s_ "Hexagrams Table") ~header: (s_ "Hexagrams Table in Fuxi Order")
		[t] )
    );

    Eliom_registration.Action.register
      ~service: save_question_action
      (fun () (q) ->
       if (String.length q) > 5 then (set_question (Some q))
       else raise  Eliom_common.Eliom_Wrong_parameter
      );

    Eliom_registration.Action.register
      ~service: save_hexa_action
      (fun _ (h) ->
       
       if (List.length h) = 6 then (set_hexagram h)
       else raise  Eliom_common.Eliom_Wrong_parameter
      );

    Iching_app.register
      ~service:question_service
      (fun () () ->
       return ( make_page ~title:(s_ "Your Question") ~header:(s_ "Your Question?")
	      [question_form ()] )
      );

      Eliom_registration.Ocaml.register 
	~service:hexa_comment_service
	(fun (hexa_no) () ->
	 Hexa_text.get_hexa_comment hexa_no);

       Eliom_registration.Ocaml.register 
	~service:hexa_judgement_comment_service
	(fun (hexa_no) () ->
	 Hexa_text.get_hexa_judgement_comment hexa_no);

        Eliom_registration.Ocaml.register 
	~service:hexa_image_comment_service
	(fun (hexa_no) () ->
	 Hexa_text.get_hexa_image_comment hexa_no);

	 Eliom_registration.Ocaml.register 
	~service:hexa_line_comment_service
	(fun (hexa_no, line_no) () ->
	 Lwt.catch
	   (fun () -> Hexa_text.get_hexa_line_comment hexa_no line_no)
	   (fun _ -> Lwt.fail Eliom_common.Eliom_Wrong_parameter)
	);

	 Eliom_registration.Any.register
	   ~service: save_result_service
	   (fun (_) () ->
	    get_current_oracle () >>=
	      fun o -> 
	       (save_oracle o) >>=
	      fun id -> Eliom_reference.set hexagram [] >>=
	      fun () -> Eliom_reference.set result_id (Some id) >>=
	      fun () ->
	      Eliom_registration.Redirection.send ~options:`Found 
						(Eliom_service.preapply 
						result_service
						(Some id))
	   )
							 
       

(**********************************************************************************************)

let _  =
  Eliom_registration.Redirection.register_service
    ~path: ["test"]
    ~get_params:Eliom_parameter.unit
    ~options: `Found
    (fun () () ->
     Eliom_reference.set question (Some "dummy") >>=
       fun () -> Eliom_reference.set hexagram [6;6;6;9;9;9] >>=
       fun () -> return (Eliom_service.preapply result_service None)
    )
    
