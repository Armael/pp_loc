
module P = Pp_loc.Position

let src = {|let rec f x =
  if x < 0 then - (f (-x))
  else x+1
|}

let input = Pp_loc.Input.string src

let loc_rec = P.of_line_col 1 5, P.of_line_col 1 8;;
let loc_if = P.of_line_col 2 3, P.of_line_col 2 5;;

Format.printf "loc is: %a@." (Pp_loc.pp ~max_lines:5 ~input) [loc_rec];;
Format.printf "loc is: %a@." (Pp_loc.pp ~max_lines:5 ~input) [loc_if];;

let loc_rec_if = P.of_line_col 1 5, P.of_line_col 2 5;;
Format.printf "loc is: %a@." (Pp_loc.pp ~max_lines:5 ~input) [loc_rec_if];;
