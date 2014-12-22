open Eliom_content.Html5.D

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

