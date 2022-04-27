module P = Pp_loc.Position

let pos_start =
  P.of_lexing Lexing.{ pos_fname = "file.ml";
                       pos_lnum = 1; pos_bol = 0; pos_cnum = 6 }
let pos_end =
  P.of_lexing Lexing.{ pos_fname = "file.ml";
                       pos_lnum = 1; pos_bol = 0; pos_cnum = 8 }

let () =
  let input = Pp_loc.Input.file "file.ml" in
  Format.printf "loc is: %a@." (Pp_loc.pp ~max_lines:5 ~input) [pos_start, pos_end]

let pos_start = P.of_line_col 1 7
let pos_end = P.of_line_col 1 9

let () =
  let input = Pp_loc.Input.file "file.ml" in
  Format.printf "loc is: %a@." (Pp_loc.pp ~max_lines:5 ~input) [pos_start, pos_end]

let () =
  let cin = open_in "file.ml" in
  let input = Pp_loc.Input.in_channel cin in
  Format.printf "loc is: %a@." (Pp_loc.pp ~max_lines:5 ~input) [pos_start, pos_end];
  (* test that reusing the in_channel works properly *)
  Format.printf "loc is: %a@." (Pp_loc.pp ~max_lines:5 ~input) [pos_start, pos_end];
  close_in cin
