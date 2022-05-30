
(** Abstraction over the input containing the input to read from. *)
module Input : sig
  (** The abstract input type.
      Informally, an "input" of type [t] should support:
      - seeking the cursor to a given initial position (as a number of bytes)
      - from this position, read the input byte per byte
  *)
  type t

  (** {2 High-level functions} *)

  (** [file fname] is an input that corresponds to the content of the file named
      [fname]. *)
  val file : string -> t

  (** [string s] is an input that corresponds to the content of the string [s].
     *)
  val string : string -> t

  (** [bytes b] is an input that corresponds to the content of the bytes array
     [b]. *)
  val bytes : bytes -> t

  (** [in_channel cin] is an input that correspond to the input channel [cin].
      The channel must correspond to a file: it must be possible to call
      [seek_in] on the channel without error.

      Creating an input using [in_channel] instead of [file] is more efficient
      if one needs to perform successive calls of [Pp_loc.pp] on the same input.
      Otherwise (or if having the best performance is not important), it is
      simpler (and thus recommended) to use [file]. *)
  val in_channel : in_channel -> t

  (** {2 Low-level functions} *)

  (** Creates an input from functions.

      @param seek a function to move to a given offset in the input.
      A call to [seek] will come before any call to [read_char].
      If the call to [seek] fails and returns [Error `Invalid_position], the
      input is considered to be unreadable and will be treated equivalenty as the
      empty input. This might happen even if the input is valid, if one tries to
      highlight an invalid location.

      @param read_char used to read the next char from the currently seeked
      position, and seek one char forward.

      @param line_offsets computes the byte offset of each line in the file.
      This is used when positions are built from only an offset, or only
      a pair [(line,column)].
  *)
  val raw :
    seek: (int -> (unit, [`Invalid_position]) result) ->
    read_char: (unit -> (char, [`End_of_input]) result) ->
    line_offsets: (unit -> int array) ->
    t

  (** [managed f] enables creating an input which lifetime is managed
     explicitely. In other words, it handles inputs with explicit open and close
     operations.

      For instance, a file (which needs to be explicitely opened then close) is
     an instance of [managed].

      If [f()] returns [(i, c)], then using the input [managed f] will first
      call [f()] to access the underlying input [i], and will terminate by
      calling [c ()].
  *)
  val managed :
    (unit -> t * (unit -> unit)) ->
    t
end

(** A position in a file or string.

    It is abstract to provide more flexibility than {!Lexing.position}.

    @since 2.0
*)
module Position : sig
  type t

  (** Convert position. The filename is ignored, the offset, line, and column
      are potentially used so they matter. *)
  val of_lexing : Lexing.position -> t

  (** Build a position from a byte offset in the input. *)
  val of_offset : int -> t

  (** [of_line_col line col] builds the position indicating the character
      at column [col] and line [line] of the input.
      Lines and columns start at 1.
  *)
  val of_line_col : int -> int -> t

  (** [shift pos n] is the position located [n] bytes after [pos]. *)
  val shift : t -> int -> t

  (** [to_offset input pos] computes the offset in bytes corresponding to
     position [pos] within [input]. *)
  val to_offset : Input.t -> t -> int

  (** [to_lexing input pos] computes the full position corresponding to position
     [pos] within input [input]. The filename is set to [filename] if specified,
     or to a dummy value otherwise (the empty string). *)
  val to_lexing : ?filename:string -> Input.t -> t -> Lexing.position
end

(** The type of source locations: start position, end position.
    The end position is not inclusive, it is just {i past} the end
    of the highlighted range. *)
type loc = Position.t * Position.t

(** Quote the parts of the input corresponding to the input locations.

   There are two different styles for highlighting errors, depending on whether
   the error fits on a single line or spans across several lines.

   For single-line errors,
{[
     foo the_error bar
]}

   gets displayed as follows, where X is the line number:
{[
     X | foo the_error bar
             ^^^^^^^^^
]}

   For multi-line errors,
{[
     foo the_
     error bar
]}
   gets displayed as:
{[
     X1 | ....the_
     X2 | error....
]}
   An ellipsis hides the middle lines of the multi-line error if it has more
   than [max_lines] lines.

   If the input list of locations is empty then this function is a no-op.
*)
val pp :
  ?max_lines:int ->
  input:Input.t ->
  Format.formatter ->
  loc list ->
  unit


(** {2 Customizing the output of [pp] (e.g. using ANSI codes)} *)

(** It is possible to customize the style of the output of [pp], for instance
   change the color of parts of the output using ANSI codes.

    This relies under the hood on [Format]'s tags mechanism, but we offer a
   slightly nicer API below. Each customizable part of the output corresponds to
   an argument of the [setup_highlight_tags] function with type [handle_tag]. A
   value of type [handle_tag] provides two functions: [open_tag] is called to
   output a string before the corresponding part of the output (e.g. an ANSI
   code that enables the desired color), and [close_tag] is called to insert a
   string after the corresponding part of the output (e.g. an ANSI code that
   resets the color to the default). *)

type handle_tag = {
  open_tag : unit -> string;
  close_tag : unit -> string;
}

(** Call this function to setup custom highlighting before using [pp].

   - [single_line_underline] corresponds to the carets ([^^^^^]) used to
   highlight the error part of the input, in the single-line case.
*)
val setup_highlight_tags :
  Format.formatter ->
  ?single_line_underline: handle_tag ->
  unit ->
  unit
