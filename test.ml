open OUnit2

let test_hexa _ =
let i1 = List.map (fun r -> List.map (fun c -> Hexa.hexa_from_index c) r) Hexa.fuxi_order in
let i2 = List.map (fun r -> List.map (fun h -> Hexa.index_from_hexa h) r) i1 in
assert_equal  i2 Hexa.fuxi_order


let test_buf_to_int _ = 
let b = Buffer.create 64 in
for i= 0 to 63 do Buffer.add_char b (char_of_int 0) done;
let n = Util.buf_to_int b in
assert_equal n 0;
Buffer.reset b;
for i= 0 to 63 do Buffer.add_char b (char_of_int 255) done;
let n = Util.buf_to_int b in
assert_equal n 0;
Buffer.reset b;
for i= 1 to 4 do Buffer.add_char b (char_of_int i) done;
let n = Util.buf_to_int b in
assert_equal (4+3*256+2*256*256+1*256*256*256) n ~printer:string_of_int;
Buffer.reset b;
for i= 1 to 64 do Buffer.add_char b (char_of_int (Random.int 255)) done;
let n = Util.buf_to_int b in
assert_bool "Not zero"  (n<>0);
assert_equal 64 (Buffer.length b)

let test_range _ =
let r = Util.range 8 in
assert_equal 8 (List.length r);
assert_equal 0 (List.hd r);
assert_equal 7 (List.fold_left (fun _ i -> i) 0 r);
let r = Util.range ~start:2 8 in
assert_equal 6 (List.length r);
assert_equal 2 (List.hd r);
assert_equal 7 (List.fold_left (fun _ i -> i) 0 r)


let test_trigram _ = 
let hexas = List.map (fun i -> Hexa.hexa_from_index i) (List.hd Hexa.fuxi_order) in
let trigrams =  List.map (fun h -> Hexa.tri_from_hex' h) hexas in
let upper = List.map (fun (_,t) -> t) trigrams in
let lower = List.map (fun (t,_) -> t) trigrams in
assert_equal [0;0;0;0;0;0;0;0] lower ~printer: (Util.list_to_string ~printer:string_of_int);
assert_equal (Util.range 8) upper  ~printer: (Util.list_to_string ~printer:string_of_int)

open I18n
open Lwt

type test_entry = {pos:int;text:string}
let test_local _ =
  let module Loc=Locale(struct
			   type entry= test_entry
			   type key=int

			   let  expected_size = 10
			   let resource_file = "test.txt"
			   let  parse_resource  s = 
			     let l = Str.split (Str.regexp "\n") s in
			     List.mapi (fun i v -> i, {pos=i;text=v}) l
				       
			 end) in
 let cwd = Sys.getcwd () in
 set_base_dir (Filename.concat cwd "test_data");
 Loc.Blocking.load_resource "en";
 let r =Loc.Blocking.get_res "en" 0 in
 assert_equal "aaaa" r.text;
 assert_raises ResourceNotAvailable (fun () -> Loc.Blocking.get_res "en" 99);
 let r =Loc.Blocking.get_res "cz" 0 in
 assert_equal "aaaa" r.text;
 Loc.Blocking.load_resource "cs";
 let r =Loc.Blocking.get_res "cs" 2 in
 assert_equal "3333" r.text;
 try
   Loc.Blocking.load_resource "xx";
   assert_failure "Should raise Sys.Sys_error"
 with
 | Sys_error _ -> ()

let test_tri_data _ =
  let cwd = Sys.getcwd () in
  set_base_dir (Filename.concat cwd "text");
  let s = get_resource_from_file_blocking "en" "trigrams.xml" in
  let ts = Tri_text.parse_xml s in
  assert_equal 8 (List.length ts) ~printer:string_of_int;
  let t = List.hd ts in
  assert_equal "Ch'ien" t.Tri_text.name

let test_hexa_data _ =
  let cwd = Sys.getcwd () in
  set_base_dir (Filename.concat cwd "text");
  let s = get_resource_from_file_blocking "en" "hexagrams.xml" in
  let ts = Hexa_text.parse_xml s in
  assert_equal 64 (List.length ts) ~printer:string_of_int;
  let t = List.hd ts in
  assert_equal "Ch'ien" t.Hexa_text.name

let test_i18n _ = 
  let cwd = Sys.getcwd () in
  init ~base:(Filename.concat cwd "test_data") ();
  let open Gettext in
  assert_equal "&<>\"" (s_ "&<>\"");
  
  assert_equal "english" (s_ "english");
  assert_equal "english 1" (Printf.sprintf (f_ "english %s") "1");
  assert_equal "english" (sn_ "english" "englishes" 1);
  assert_equal "englishes" (sn_ "english" "englishes" 2);
  assert_equal "english 1" (Printf.sprintf (fn_ "english %s" "englishes %s" 1) "1");
  assert_equal "englishes 2" (Printf.sprintf (fn_ "english %s" "englishes %s" 2) "2");

  init ~lang:"cs" ~base:(Filename.concat cwd "test_data") ();
  assert_equal "čeština" (s_ "english");
  assert_equal "čeština 1" (Printf.sprintf (f_ "english %s") "1");
  assert_equal "čeština" (sn_ "english" "englishes" 1);
  assert_equal "češtiny" (sn_ "english" "englishes" 2);
  assert_equal "čeština 1" (Printf.sprintf (fn_ "english %s" "englishes %s" 1) "1");
  assert_equal "češtiny 2" (Printf.sprintf (fn_ "english %s" "englishes %s" 2) "2")
  
  

let tests = "All_tests" >:::
["test_hexa" >:: test_hexa;
"test_buf_to_int" >:: test_buf_to_int;
"test_range" >:: test_range;
"test_trigram" >:: test_trigram;
"test_local" >:: test_local;
"test_tri_data" >:: test_tri_data;
"test_hexa_data" >:: test_hexa_data;
"test_i18n" >:: test_i18n;
]

let () =
run_test_tt_main tests



