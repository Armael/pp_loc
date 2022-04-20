
module P = Pp_loc.Position

let src = {|
let rec f x =
  if x < 0 then - (f (-x))
  else x+1

|}

let input = Pp_loc.Input.string src

let loc1 = P.of_line_col 2 5, P.of_line_col 2 8;;

Format.printf "loc is: %a@." (Pp_loc.pp ~max_lines:5 ~input) [loc1];;
