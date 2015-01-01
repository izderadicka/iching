{shared{
  open Eliom_lib
  open Eliom_content
  open Html5
  open Html5.D
}}

{client{
open Client
open Lwt
   }}

open Main
open Lwt
open I18n.Gettext

let make_tri_section t =
let module Html5 = F in
<:html5list< 
  <span class="trigram-name">$str:t.Tri_text.name$</span>
  <span class="trigram-translated">$str:t.Tri_text.translated$</span>
>>

let make_info_btn svc hexa_no   =
let close_msg = (s_ "Close") in
F.span ~a:[a_class ["more-info"]; 
	  a_onclick {{show_info_handler (fun () ->
			   (Eliom_client.call_ocaml_service 
				       ~service:%svc
				     %hexa_no ())
					) 
					%close_msg
	 }}]
      [] 

let make_info_btn_lines hexa_no (line_no:int)  =
let close_msg= (s_ "Close") in
F.span ~a:[a_class ["more-info"]; 
	  a_onclick {{show_info_handler 
			  (fun () -> (Eliom_client.call_ocaml_service 
				       ~service:%hexa_line_comment_service
				     (%hexa_no,%line_no)  ())
			  )
			  %close_msg
	 }}]
      [] 

let make_lines_section lines hexa_no =
let open Hexa_text in
let module Html5 = F in
 let make_line l =
   let line_no:int = l.L.no in
   <<
    <li class="hexa-line">
    <h3 class="line-title">$str:l.L.change$
    $make_info_btn_lines hexa_no line_no$
    </h3>
    <p class="predication">$list:break_lines l.L.predication$</p>
    </li>
    >>
in 
match lines with 
| [] -> div ~a:[a_class ["centered";"section"]] []
| _ -> let open F in
       div ~a:[a_class ["centered";"section"]]
   [  h2 [pcdata (s_ "Moving Lines")];
      ul ~a:[a_class ["lines-list"]] (List.map (fun l -> make_line l) lines)]

let make_hexa_section ?(cls="") h hh =
  let module Html5 = F in
  let hexa_no = Hexa.index_from_hexa hh in
  let h1,h2 =  Hexa.tri_from_hex hh in
  let cls = "hexa-name "^ cls in
  let open Hexa_text in
  let hexa_cont = D.div ~a:[a_class ["hexa-svg"; "big"]; 
			    Eliom_content.Html5.Custom_data.attrib Common.a_hexa h] 
			[] in
  lwt h_below = Tri_text.get_trigram_text h1 and h_above=Tri_text.get_trigram_text h2 in
  lwt ht = get_hexagram_text hexa_no in
  ignore {unit{ add_hexa_svg %hexa_cont %hh}};
  return
<<
 <div class="hexagram centered section">
	   <h2 class=$cls$> $str:string_of_int hexa_no$. 
		   $str:ht.Hexa_text.name$ - 
		   $str:ht.Hexa_text.translated$
           $make_info_btn hexa_comment_service hexa_no$
          </h2>
	  <div class="hexa-info centered">
          $hexa_cont$
	 
          <div class="trigram above">
          $list:make_tri_section h_above$
          </div>
	  <div class="trigram below">
          $list:make_tri_section h_below$</div>
	  </div>
         
          <h3>$str:(s_ "The Judgement")$
          $make_info_btn hexa_judgement_comment_service hexa_no $
          </h3>
          <p class="predication">$list:break_lines ht.judgement.J.predication$</p>
	  <h3>$str:(s_ "The Image")$
          $make_info_btn hexa_image_comment_service hexa_no $
          </h3>
           <p class="predication">$list:break_lines ht.image.J.predication$</p>
	  </div>	
>>

let saved_result_note id ts =
  match id with 
  | None -> []
  | Some _ -> [F.p [F.pcdata (Printf.sprintf (f_ "This is saved result from %s. You can link it or share the link.") (I18n.fmt_calendar ts))]]

let save_result_link id =
  let open F in 
  let l =  div ~a:[a_class ["next-btn"];
		  a_onclick {{fun _e -> 

			     Eliom_client.change_url
			       ~service: %result_service
					    (Some "XXX")
			     }}
			    ] [pcdata (s_ "Remember It")] in
  l
  

let get_oracle_params = function
  | None ->  get_current_oracle ()
  | Some id -> catch (fun () -> get_stored_oracle id)
		     (function 
		       | Not_found -> raise Eliom_common.Eliom_404
		       | e -> raise e)

let redir_if_needed  id_in_url main =
  Eliom_reference.get result_id >>=
    fun id ->
    match (id_in_url, id) with
    | (None, Some id) -> 
         ignore {unit{
		     ignore (Eliom_client.change_page ~service:%result_service (Some %id) ())
		   }};
         return (make_page ~title:(s_ "Redirecting to stored result  ")
		   ~header:(s_ "You should use URL with stored result ID")
		   [])
    | _ -> main id_in_url


let result_main stored_result_id = 
  get_oracle_params stored_result_id >>=
    (fun (q,h,ts) -> 
     let module Html5 = F  in
     let hh = Hexa.hexa_from_numbers h in
     let snd,snd_exists = Hexa.complementary_hexa hh in
     let hexa_no = (Hexa.index_from_hexa hh) in
     
     lwt h1 = make_hexa_section ~cls:"primary" h hh
    and h2 = if snd_exists then make_hexa_section ~cls:"secondary" (Hexa.numbers_from_hexa snd) snd 
	     else return (F.div [])
     in
     lwt lines = Hexa_text.get_lines hexa_no (Hexa.moving_lines hh) in
     
      return  ( make_page ~title:((s_ "I Ching Result from ") ^ (I18n.fmt_calendar ts))
			  ~header:(s_ "I Ching Oracle")
		   
	  <:html5list<
	  <div class="saved-result centered">$list:saved_result_note stored_result_id ts$</div>
	  <div class="question-statement centered section">
	  <h2>$str:(s_ "Your Question Was")$</h2>
	  <p>$str:q$</p>
	  </div>
    	 
	  $h1$
	  $make_lines_section lines hexa_no $
	  $h2$
	  $save_result_form stored_result_id$
	  $F.a ~a:[a_class ["next-btn"]] ~service:question_service [pcdata (s_ "New Question")] ()$
	  >>
	    )
	 )

let language_selector () = 
  let lang = I18n.get_current_lang () in
  let s = D.Raw.select 
	    ~a:[a_name "lang";
		a_onchange {{fun e -> 
			     let select = (Js.coerce_opt (e##target) 
							 Dom_html.CoerceTo.select 
							 (fun _ -> assert false))
			     in
			     let l = Js.to_string (select##value) in
			     async (fun () -> Eliom_client.change_page ~service:%change_lang_service l ())
			   }}
	       ]
	    (List.map (fun (l,lname) -> F.Raw.option ~a: ([a_value l]@
							 (if l=lang then [a_selected `Selected ] else []))
						     
						     (pcdata lname)) !available_langs)
  in
  F.div ~a:[a_class ["lang-selector centered"]] [F.Raw.label [pcdata (s_ "Language: ");s]]

let jj_counter () = 
  let cjin = D.div ~a:[a_class ["counter";"jin"]] [] in
  let cjang = D.div ~a:[a_class ["counter";"jang"]] [] in
  ignore {unit{
	      launch_counter %cjin %cjang %csig_jin %csig_jang
	    }};
  F.div ~a:[a_class ["jj centered"]] 
	[F.div ~a:[a_class ["jj-picture"]] [cjang;cjin];
	F.h2 [pcdata (s_ "Yin - Yang Counter")];]

let ()  = 
 Iching_app.register
    ~service:main_service
    (fun () () ->
      let open Html5.F in 
     
      return (make_page ~title:(s_ "I Ching - Book of Changes") ~header:((s_ "I Ching")^" (易經) "^(s_"Book of Changes"))
			[ language_selector ();
			  div ~a:[a_class ["intro"; "centered"]]
			   [p 
			   [pcdata (s_ "This interactive application presents ") ;
			    make_ext_link "http://en.wikipedia.org/wiki/I_Ching" (s_ "I Ching ");
			    pcdata (s_ " oracle.");
			    br ();
			    pcdata (s_ "Application is written in ");
			    make_ext_link "http://ocaml.org" "Ocaml ";
			    pcdata (s_ " language and web framework ");
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
			 a ~service:question_service ~a:[a_class ["next-btn"]] [pcdata (s_ "Start Here")] ();
			 jj_counter ();
			]
	     )
    );

 Iching_app_checked.register
   ~service: coins_service
  (fun () () ->
   
   let coins_area , remaining, coins = make_coins_area () in
   let progress_bar = div ~a:[a_class ["progress-area"]] [] in
   let throws_area = ul ~a:[a_class ["throws-area"]] [] in
   let next_btn = hexa_form () in
    ignore {unit{ 
       launch_coins %coins_area %progress_bar %remaining %coins %throws_area %next_btn
      }};
   return (make_page ~title:(s_ "Throw coins") ~header:(s_ "Throw coins") 
		     [progress_bar;
		       coins_area;
		       div ~a:[a_class ["throws-wrap"]] [throws_area];
		       next_btn])
  );

 Iching_app_checked.register
   ~service:result_service
   (fun (stored_result_id) () ->
    redir_if_needed stored_result_id result_main
   );

 Iching_app.register
   ~service: hexa_info_service
   (fun (hexa_no) () ->
    let hh = Hexa.hexa_from_index hexa_no in
    let h = Hexa.numbers_from_hexa hh in
    lwt hp =  make_hexa_section ~cls:"any" h hh in
    lwt ht = Hexa_text.get_hexagram_text hexa_no in
    return (make_page ~title: (Printf.sprintf (f_ "Hexagram no.%d") hexa_no)
	   ~header: (s_ "Hexagram")
	   ([hp;
	     make_lines_section ht.Hexa_text.lines hexa_no]
	   ))
   )

let _ = Iching_app.register_service
	  ~path: ["xtest"]
	  ~get_params: Eliom_parameter.(suffix (int "h"))
	 
	  (fun hno () ->
	   let hh= Hexa.hexa_from_index hno in
	   let _h = Hexa.numbers_from_hexa hh in
	   let hexa_cont = Common.hexa_svg hh in
	   let client_side = D.div ~a:[a_class ["on_client"]] [] in
	   ignore {unit{ add_hexa_svg %client_side %hh}};
	   return (make_page ~title:"Test" ~header:"Test SVG"
			     [hexa_cont;
			     F.h2 [F.pcdata "On Client"];
			     client_side]
		  )
	   
	  ) 

  



	 
	  
