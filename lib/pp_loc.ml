
let rec list_find_map f = function
  | [] -> None
  | x :: xs ->
    begin match f x with
      | None -> list_find_map f xs
      | Some _ as y -> y
    end

let list_filter_map f =
  let rec aux accu = function
    | [] -> List.rev accu
    | x :: l ->
      match f x with
      | None -> aux accu l
      | Some v -> aux (v :: accu) l
  in
  aux []

let option_fold ~none ~some = function Some v -> some v | None -> none

let pp_two_columns ?(sep = "|") ?max_lines ppf (lines: (string * string) list) =
  let left_column_size =
    List.fold_left (fun acc (s, _) -> max acc (String.length s)) 0 lines in
  let lines_nb = List.length lines in
  let ellipsed_first, ellipsed_last =
    match max_lines with
    | Some max_lines when lines_nb > max_lines ->
        let printed_lines = max_lines - 1 in (* the ellipsis uses one line *)
        let lines_before = printed_lines / 2 + printed_lines mod 2 in
        let lines_after = printed_lines / 2 in
        (lines_before, lines_nb - lines_after - 1)
    | _ -> (-1, -1)
  in
  Format.fprintf ppf "@[<v>";
  List.iteri (fun k (line_l, line_r) ->
    if k = ellipsed_first then Format.fprintf ppf "...@,";
    if ellipsed_first <= k && k <= ellipsed_last then ()
    else Format.fprintf ppf "%*s %s %s@," left_column_size line_l sep line_r
  ) lines;
  Format.fprintf ppf "@]"

(******************************************************************************)

module Input = struct
  type raw = {
    seek : int -> (unit, [`Invalid_position]) result;
    read_char : unit -> (char, [`End_of_input]) result;
    line_offsets : int array lazy_t;
  }

  type t =
    | Raw of raw
    | Managed of (unit -> t * (unit -> unit))

  let raw ~seek ~read_char ~line_offsets =
    let line_offsets = lazy (line_offsets()) in
    Raw { seek; read_char; line_offsets; }

  let managed create = Managed create

  let in_channel cin =
    let seek i =
      try Ok (seek_in cin i) with Sys_error _ -> Error `Invalid_position in
    let read_char () =
      try Ok (input_char cin)
      with End_of_file | Sys_error _ -> Error `End_of_input in
    let line_offsets = lazy (Line_offsets.from_chan cin) in
    Raw { seek; read_char; line_offsets }

  let file name =
    managed (fun () ->
      let cin = open_in_bin name in
      in_channel cin, (fun () -> close_in cin))

  let buf
    (b: 'a)
    ~(len: 'a -> int)
    ~(read: 'a -> int -> char)
    ~(to_string : 'a -> string)
    =
    let cur_pos = ref (-1) in
    let inbound i = i >= 0 && i < len b in
    let seek i =
      if inbound i then
        Ok (cur_pos := i)
      else
        Error `Invalid_position in
    let read_char () =
      if not (inbound !cur_pos) then Error `End_of_input
      else
        let c = read b !cur_pos in
        incr cur_pos;
        Ok c
    in
    let line_offsets = lazy (Line_offsets.from_string (to_string b)) in
    Raw { seek; read_char; line_offsets }

  let string s = buf s ~len:String.length ~read:String.get ~to_string:(fun s -> s)
  let bytes b = buf b ~len:Bytes.length ~read:Bytes.get ~to_string:Bytes.unsafe_to_string

  (*****)

  let rec open_raw = function
    | Raw r -> r, (fun () -> ())
    | Managed o ->
      let i, c = o () in
      let r, c' = open_raw i in
      r, (fun () -> c' (); c ())
end


(******************************************************************************)
(* Positions and locations *)

module Position = struct
  type t =
    | Offset of int
    | Line_col of int * int
    | Full of Lexing.position
    | Shift of t * int

  let of_lexing pos = Full pos
  let of_offset i = Offset i
  let of_line_col l c = Line_col (l, c)
  let shift l n = Shift (l, n)

  let rec to_offset_raw (input:Input.raw) (self:t) : int =
    match self with
    | Offset offset -> offset
    | Full pos -> pos.Lexing.pos_cnum
    | Line_col (line, col) ->
      let lazy index = input.Input.line_offsets in
      Line_offsets.find_offset index ~line ~col
    | Shift (pos, n) -> to_offset_raw input pos + n

  let to_offset (input:Input.t) (self:t) : int =
    let input, close = Input.open_raw input in
    let off = to_offset_raw input self in
    close ();
    off

  let rec to_lexing_raw (input:Input.raw) (self:t) : Lexing.position =
    match self with
    | Full pos -> pos
    | Offset offset ->
      let lazy index = input.Input.line_offsets in
      let line, col = Line_offsets.line_col_of_offset index offset in
      {Lexing.pos_cnum=offset; pos_lnum=line; pos_bol=offset-col;
       pos_fname="";}
    | Line_col (line, col) ->
      let lazy index = input.Input.line_offsets in
      let offset = Line_offsets.find_offset index ~line ~col in
      let bol = offset - (col - 1) (* columns are 1-based *) in
      {Lexing.pos_cnum=offset; pos_lnum=line; pos_bol=bol;
       pos_fname="";}
    | Shift (pos, n) ->
      to_lexing_raw input (Offset (to_offset_raw input pos + n))

  let to_lexing ?(filename="") (input:Input.t) (self:t) : Lexing.position =
    let input, close = Input.open_raw input in
    let pos = to_lexing_raw input self in
    close ();
    { pos with pos_fname = filename }
end

type loc = Position.t * Position.t

(******************************************************************************)
(* Format highlighting tags *)

type Format.stag +=
   | Single_line_underline_tag

type handle_tag = {
  open_tag : unit -> string;
  close_tag : unit -> string;
}

let handle_tag_default = {
  open_tag = (fun () -> "");
  close_tag = (fun () -> "");
}

let setup_highlight_tags ppf
    ?(single_line_underline = handle_tag_default)
    ()
  =
  let mark_open_stag ~or_else = function
    | Single_line_underline_tag ->
        single_line_underline.open_tag ()
    | stag ->
      or_else stag
  in
  let mark_close_stag ~or_else = function
    | Single_line_underline_tag ->
      single_line_underline.close_tag ()
    | stag ->
      or_else stag
  in
  let functions = Format.pp_get_formatter_stag_functions ppf () in
  let functions' = {
    functions with
    mark_open_stag = (mark_open_stag ~or_else:functions.mark_open_stag);
    mark_close_stag = (mark_close_stag ~or_else:functions.mark_open_stag);
  } in
  Format.pp_set_mark_tags ppf true; (* enable tags *)
  Format.pp_set_formatter_stag_functions ppf functions';
  ()

(******************************************************************************)
(* An interval set structure; additionally, it stores user-provided information
   at interval boundaries.

   The implementation provided here is naive and assumes the number of intervals
   to be small, but the interface would allow for a more efficient
   implementation if needed.

   Note: the structure only stores maximal intervals (that therefore do not
   overlap).
*)

module ISet : sig
  type 'a bound = 'a * int
  type 'a t
  (* bounds are included *)
  val of_intervals : ('a bound * 'a bound) list -> 'a t

  val mem : 'a t -> pos:int -> bool
  val find_bound_in : 'a t -> range:(int * int) -> 'a bound option

  val is_start : 'a t -> pos:int -> 'a option
  val is_end : 'a t -> pos:int -> 'a option

  val extrema : 'a t -> ('a bound * 'a bound) option
end
=
struct
  type 'a bound = 'a * int

  (* non overlapping intervals *)
  type 'a t = ('a bound * 'a bound) list

  let of_intervals intervals =
    let pos =
      List.map (fun ((a, x), (b, y)) ->
        if x > y then [] else [((a, x), `S); ((b, y), `E)]
      ) intervals
      |> List.flatten
      |> List.sort (fun ((_, x), k) ((_, y), k') ->
        (* Make `S come before `E so that consecutive intervals get merged
           together in the fold below *)
        let kn = function `S -> 0 | `E -> 1 in
        compare (x, kn k) (y, kn k'))
    in
    let nesting, acc =
      List.fold_left (fun (nesting, acc) (a, kind) ->
        match kind, nesting with
        | `S, `Outside -> `Inside (a, 0), acc
        | `S, `Inside (s, n) -> `Inside (s, n+1), acc
        | `E, `Outside -> assert false
        | `E, `Inside (s, 0) -> `Outside, ((s, a) :: acc)
        | `E, `Inside (s, n) -> `Inside (s, n-1), acc
      ) (`Outside, []) pos in
    assert (nesting = `Outside);
    List.rev acc

  let mem iset ~pos =
    List.exists (fun ((_, s), (_, e)) -> s <= pos && pos <= e) iset

  let find_bound_in iset ~range:(start, end_)  =
    list_find_map (fun ((a, x), (b, y)) ->
      if start <= x && x <= end_ then Some (a, x)
      else if start <= y && y <= end_ then Some (b, y)
      else None
    ) iset

  let is_start iset ~pos =
    list_find_map (fun ((a, x), _) ->
      if pos = x then Some a else None
    ) iset

  let is_end iset ~pos =
    list_find_map (fun (_, (b, y)) ->
      if pos = y then Some b else None
    ) iset

  let extrema iset =
    if iset = [] then None
    else Some (fst (List.hd iset), snd (List.hd (List.rev iset)))
end

type input_line = {
  text : string;
  start_pos : int;
}

(* Takes a list of lines with possibly missing line numbers.

   If the line numbers that are present are consistent with the number of lines
   between them, then infer the intermediate line numbers.

   This is not always the case, typically if lexer line directives are
   involved... *)
let infer_line_numbers
    (lines: (int option * input_line) list):
  (int option * input_line) list
  =
  let (_, offset, consistent) =
    List.fold_left (fun (i, offset, consistent) (lnum, _) ->
      match lnum, offset with
      | None, _ -> (i+1, offset, consistent)
      | Some n, None -> (i+1, Some (n - i), consistent)
      | Some n, Some m -> (i+1, offset, consistent && n = m + i)
    ) (0, None, true) lines
  in
  match offset, consistent with
  | Some m, true ->
      List.mapi (fun i (_, line) -> (Some (m + i), line)) lines
  | _, _ ->
      lines

(* [get_lines] must return the lines to highlight, given starting and ending
   positions.

   See [lines_around_from_file] below for an instantiation of [get_lines] that
   reads from a file.
*)
let highlight_quote ppf
    ~(get_lines: start_pos:Lexing.position -> end_pos:Lexing.position -> input_line list)
    ~(max_lines: int)
    locs
  =
  let open Lexing in
  let iset = ISet.of_intervals @@ list_filter_map (fun (s, e) ->
    if s.pos_cnum = -1 || e.pos_cnum = -1 then None
    else Some ((s, s.pos_cnum), (e, e.pos_cnum - 1))
  ) locs in
  match ISet.extrema iset with
  | None -> ()
  | Some ((leftmost, _), (rightmost, _)) ->
      let lines =
        get_lines ~start_pos:leftmost ~end_pos:rightmost
        |> List.map (fun ({ text; start_pos } as line) ->
          let end_pos = start_pos + String.length text - 1 in
          let line_nb =
            match ISet.find_bound_in iset ~range:(start_pos, end_pos) with
            | None -> None
            | Some (p, _) -> Some p.pos_lnum
          in
          (line_nb, line))
        |> infer_line_numbers
        |> List.map (fun (lnum, { text; start_pos }) ->
          (text,
           option_fold ~some:string_of_int ~none:"" lnum,
           start_pos))
      in
    Format.fprintf ppf "@[<v>";
    begin match lines with
    | [] | [("", _, _)] -> ()
    | [(line, line_nb, line_start_cnum)] ->
        (* Single-line error *)
        Format.fprintf ppf "%s | %s@," line_nb line;
        Format.fprintf ppf "%*s   " (String.length line_nb) "";
        for pos = line_start_cnum to rightmost.pos_cnum - 1 do
          if ISet.is_start iset ~pos <> None then
            Format.pp_open_stag ppf Single_line_underline_tag;
          if ISet.mem iset ~pos then Format.pp_print_char ppf '^'
          else Format.pp_print_char ppf ' ';
          if ISet.is_end iset ~pos <> None then
            Format.pp_close_stag ppf ();
        done;
        Format.pp_close_stag ppf (); (* XXX ? *)
        Format.fprintf ppf "@,"
    | _ ->
        (* Multi-line error *)
        pp_two_columns ~sep:"|" ~max_lines ppf
        @@ List.map (fun (line, line_nb, line_start_cnum) ->
          let line = String.mapi (fun i car ->
            if ISet.mem iset ~pos:(line_start_cnum + i) then car else '.'
          ) line in
          (line_nb, line)
        ) lines
    end;
    Format.fprintf ppf "@]"

let lines_around
    ~(start_pos: Lexing.position) ~(end_pos: Lexing.position)
    ~(input: Input.raw):
  input_line list
  =
  let lines = ref [] in
  let bol = ref start_pos.pos_bol in
  let cur = ref start_pos.pos_bol in
  let b = Buffer.create 80 in
  let add_line () =
    if !bol < !cur then begin
      let text = Buffer.contents b in
      Buffer.clear b;
      lines := { text; start_pos = !bol } :: !lines;
      bol := !cur
    end
  in
  let rec loop () =
    if !bol >= end_pos.pos_cnum then ()
    else begin
      match input.read_char () with
      | Error `End_of_input ->
          add_line ()
      | Ok c ->
          incr cur;
          match c with
          | '\r' -> loop ()
          | '\n' -> add_line (); loop ()
          | _ -> Buffer.add_char b c; loop ()
    end
  in
  match input.seek start_pos.pos_bol with
  | Ok () ->
    loop ();
    List.rev !lines
  | Error `Invalid_position ->
    []

let pp
    ?(max_lines = 10)
    ~(input : Input.t)
    (ppf: Format.formatter)
    (locs: loc list)
  =

  let input_raw, close_input = Input.open_raw input in

  (* convert locations so that the positions are full lexing positions *)
  let locs = match locs with
    | [] -> []
    | _ ->
      let locs =
        List.map
          (fun (start_pos,end_pos) ->
             Position.to_lexing_raw input_raw start_pos,
             Position.to_lexing_raw input_raw end_pos)
          locs
      in
      locs
  in

  (* The fact that [get_lines] is only called once by [highlight_quote] is
     important here, because it might consume the input... *)
  let get_lines ~start_pos ~end_pos =
    let lines = lines_around ~start_pos ~end_pos ~input:input_raw in lines
  in
  highlight_quote ppf ~get_lines ~max_lines locs;
  close_input ();
  ()
