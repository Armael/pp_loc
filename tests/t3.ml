module P = Pp_loc.Position

let src = {|let foo () =
  42 +. 1.
;;
|}

let input = Pp_loc.Input.string src

let pos_start =
  P.of_lexing Lexing.{ pos_fname = ""; pos_lnum = 2; pos_bol = 13; pos_cnum = 15 }
let pos_end =
  P.of_lexing Lexing.{ pos_fname = ""; pos_lnum = 2; pos_bol = 13; pos_cnum = 17 }

let () =
  Format.printf "loc is: %a@." (Pp_loc.pp ~max_lines:5 ~input) [pos_start, pos_end]

let pos_start = P.of_line_col 2 3
let pos_end = P.of_line_col 2 5

let () =
  Format.printf "loc is: %a@." (Pp_loc.pp ~max_lines:5 ~input) [pos_start, pos_end]

let pos_start = P.of_offset 15
let pos_end = P.of_offset 17

let () =
  Format.printf "loc is: %a@." (Pp_loc.pp ~max_lines:5 ~input) [pos_start, pos_end]

let pos_start = P.of_line_col 2 3
let pos_end = P.shift pos_start 2

let () =
  Format.printf "loc is: %a@." (Pp_loc.pp ~max_lines:5 ~input) [pos_start, pos_end]



let src = {|let x =
 (1
  + 2) +. 3.;;
|}

let input = Pp_loc.Input.string src

let pos_start =
  P.of_lexing Lexing.{ pos_fname = ""; pos_lnum = 2; pos_bol = 8; pos_cnum = 9 }
let pos_end =
  P.of_lexing Lexing.{ pos_fname = ""; pos_lnum = 3; pos_bol = 12; pos_cnum = 18 }

let () =
  Format.printf "loc is: %a@." (Pp_loc.pp ~max_lines:5 ~input) [pos_start, pos_end]

let pos_start = P.of_line_col 2 2
let pos_end = P.of_line_col 3 7

let () =
  Format.printf "loc is: %a@." (Pp_loc.pp ~max_lines:5 ~input) [pos_start, pos_end]

let pos_start = P.of_offset 9
let pos_end = P.of_offset 18

let () =
  Format.printf "loc is: %a@." (Pp_loc.pp ~max_lines:5 ~input) [pos_start, pos_end]

let pos_start = P.of_line_col 2 2
let pos_end = P.shift pos_start 9

let () =
  Format.printf "loc is: %a@." (Pp_loc.pp ~max_lines:5 ~input) [pos_start, pos_end]
