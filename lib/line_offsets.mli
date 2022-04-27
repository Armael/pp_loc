
(* compute line offsets *)

type t = int array

val from_chan : in_channel -> t

val from_string : string -> t

val find_line_offset : t -> line:int -> int

val find_offset : t -> line:int -> col:int -> int

val line_col_of_offset : t -> int -> int * int
