
(* A very basic dynamic array. *)
module Vec = struct
  type 'a t = {
    mutable arr: 'a array;
    mutable sz: int;
  }

  let create() : _ t = { arr=[||]; sz=0 }

  let grow_ self x =
    let new_size = max 6 (let n = Array.length self.arr in n + n / 2 + 1) in
    let arr = Array.make new_size x in
    Array.blit self.arr 0 arr 0 self.sz;
    self.arr <- arr

  let push (self:_ t) x =
    if self.sz = Array.length self.arr then grow_ self x;
    self.arr.(self.sz) <- x;
    self.sz <- 1 + self.sz

  let to_array self = Array.sub self.arr 0 self.sz
end

(* maps i to the offset of the beginning of the i-th line *)
type t = int array

let from_string (s:string) : t =
  let lines = Vec.create() in
  Vec.push lines 0; (* first line is easy *)
  let size = String.length s in
  let i = ref 0 in
  while !i < size do
    match String.index_from_opt s !i '\n' with
    | None -> i := size
    | Some j ->
      Vec.push lines (j+1);
      i := j+1;
  done;
  Vec.to_array lines

let from_chan (cin:in_channel) : t =
  let buf = Buffer.create 1024 in
  (try seek_in cin 0 with _ -> ());
  (try while true do Buffer.add_channel buf cin (16 * 1024) done; assert false
   with End_of_file -> ());
  from_string (Buffer.contents buf)

let find_line_offset (self:t) ~line : int =
  let line = line-1 in (* lines are 1-based *)
  if line >= Array.length self then (
    Array.length self
  ) else (
    self.(line)
  )

let find_offset (self:t) ~line ~col : int =
  let col = col-1 in (* columns are 1-based *)
  let off = find_line_offset self ~line in
  off + col

let line_col_of_offset (self:t) (off:int) : int * int =
  (* binary search *)
  let low = ref 0 in
  let high = ref (Array.length self) in
  let continue = ref true in
  while !continue && !low < !high do
    let middle = !low / 2 + !high / 2 in
    let off_middle = self.(middle) in

    if off_middle <= off then (
      if middle + 1 = Array.length self ||
         self.(middle + 1) > off then (
        (* found the range *)
        low := middle;
        continue := false;
      ) else (
        low := middle + 1;
      )
    ) else (
      high := middle - 1;
    )
  done;
  let col = off - self.(!low) in
  let line = !low + 1 in
  line, col
;;
