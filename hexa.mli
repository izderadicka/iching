(** functions to manipulate hexagrams *) 

val fuxi_order : int list list

type line = Full of bool | Broken of bool
type hexagram = line list
type trigram = line list
val hexa_from_index : int -> line list
val index_from_hexa : line list -> int
val tri_from_hex' : line list -> int * int
val tri_from_hex : line list -> int * int
val hexa_from_numbers : int list -> line list
val numbers_from_hexa : line list -> int list
val complementary_hexa : line list -> line list * bool
val moving_lines : line list -> int list
