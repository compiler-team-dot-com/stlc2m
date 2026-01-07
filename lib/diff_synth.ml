type hunk = { a_start : int; a_len : int; b_start : int; b_len : int }
(* Interpretation:
     Old text replaces a[a_start .. a_start+a_len]
     With b[b_start .. b_start+b_len]. *)

let split_lines_keep_ends (s : string) : string array =
  (* Keep trailing '\n' with each line so replacements reconstitute cleanly. *)
  let n = String.length s in
  let rec go i acc =
    if i >= n then List.rev acc
    else
      match String.index_from_opt s i '\n' with
      | None ->
          let line = String.sub s i (n - i) in
          List.rev (line :: acc)
      | Some j ->
          let line = String.sub s i (j - i + 1) in
          go (j + 1) (line :: acc)
  in
  Array.of_list (go 0 [])

(* Myers diff on arrays of strings. Returns a list of hunks. *)
(* Myers diff on arrays of strings. Returns a list of hunks. *)
let myers_hunks (a : string array) (b : string array) : hunk list =
  let n = Array.length a in
  let m = Array.length b in
  let maxd = n + m in
  let offset = maxd in

  (* v.(k+offset) = furthest x reached on diagonal k, at current d.
     Myers expects both k=0 and k=1 to be reachable at d=0 so the first
     insertion step doesn't read an unreachable diagonal. *)
  let v = Array.make ((2 * maxd) + 1) (-1) in
  v.(offset) <- 0;
  v.(offset + 1) <- 0;

  (* trace.(d) stores v AFTER finishing all k at edit distance d. *)
  let trace = Array.make (maxd + 1) [||] in

  let rec snake x y =
    if x < n && y < m && String.equal a.(x) b.(y) then snake (x + 1) (y + 1)
    else (x, y)
  in

  let rec forward d =
    (* compute v for this d based on values from previous d *)
    let v_prev = Array.copy v in

    (* k runs in steps of 2: -d, -d+2, ..., d *)
    let k = ref (-d) in
    while !k <= d do
      let kk = !k in
      let ki = kk + offset in

      (* Choose whether last step was down (insertion) or right (deletion). *)
      let x =
        if kk = -d then
          (* must come from k+1 (down) *)
          v_prev.(ki + 1)
        else if kk = d then
          (* must come from k-1 (right) *)
          v_prev.(ki - 1) + 1
        else
          let down = v_prev.(ki + 1) in
          let right = v_prev.(ki - 1) + 1 in
          if right > down then right else down
      in
      let y = x - kk in

      (* Extend along snake of equal lines. *)
      let x', _y' = snake x y in
      v.(ki) <- x';

      k := kk + 2
    done;

    (* Save snapshot for backtracking. *)
    trace.(d) <- Array.copy v;

    (* Terminate if we reached the end on diagonal k = n - m *)
    if v.(n - m + offset) >= n then d else forward (d + 1)
  in

  let d_final = forward 0 in

  (* Backtrack to produce hunks. *)
  let rec backtrack d x y acc =
    if d = 0 then acc
    else
      let v_prev = trace.(d - 1) in
      let k = x - y in
      let ki = k + offset in

      (* Determine which predecessor diagonal we came from. *)
      let prev_k =
        if k = -d || (k <> d && v_prev.(ki - 1) < v_prev.(ki + 1)) then k + 1
        else k - 1
      in

      let prev_x = v_prev.(prev_k + offset) in
      let prev_y = prev_x - prev_k in

      (* Undo snake: move back over equal lines. *)
      let rec unsnake x y =
        if x > prev_x && y > prev_y && String.equal a.(x - 1) b.(y - 1) then
          unsnake (x - 1) (y - 1)
        else (x, y)
      in
      let x0, y0 = unsnake x y in

      (* The edit step is from (prev_x, prev_y) to (x0, y0). *)
      let h =
        {
          a_start = prev_x;
          a_len = x0 - prev_x;
          b_start = prev_y;
          b_len = y0 - prev_y;
        }
      in
      backtrack (d - 1) prev_x prev_y (h :: acc)
  in

  backtrack d_final n m []
  |> List.filter (fun h -> h.a_len <> 0 || h.b_len <> 0)

(* Convert hunks to line-span replacements. Coalesce adjacent hunks. *)
let coalesce (hs : hunk list) : hunk list =
  let rec go acc = function
    | [] -> List.rev acc
    | h :: tl -> (
        match acc with
        | h0 :: acc_tl ->
            let a_end0 = h0.a_start + h0.a_len in
            let b_end0 = h0.b_start + h0.b_len in
            if h.a_start <= a_end0 && h.b_start <= b_end0 then
              let a_end = max a_end0 (h.a_start + h.a_len) in
              let b_end = max b_end0 (h.b_start + h.b_len) in
              let h' =
                {
                  a_start = h0.a_start;
                  a_len = a_end - h0.a_start;
                  b_start = h0.b_start;
                  b_len = b_end - h0.b_start;
                }
              in
              go (h' :: acc_tl) tl
            else go (h :: acc) tl
        | [] -> go [ h ] tl)
  in
  go [] hs

(* Precompute old-text line start offsets (in bytes). *)
let line_starts (old_lines : string array) : int array =
  let n = Array.length old_lines in
  let starts = Array.make (n + 1) 0 in
  for i = 0 to n - 1 do
    starts.(i + 1) <- starts.(i) + String.length old_lines.(i)
  done;
  starts

let pos_of_offset ~(line_starts : int array) ~(offset : int) : int * int =
  (* line is 1-based, col is 0-based *)
  let n = Array.length line_starts - 1 in
  if n = 0 then (1, offset)
  else
  (* binary search: largest i s.t. line_starts.(i) <= offset *)
  let rec bs lo hi =
    if lo + 1 >= hi then lo
    else
      let mid = (lo + hi) / 2 in
      if line_starts.(mid) <= offset then bs mid hi else bs lo mid
  in
  let i = bs 0 (n + 1) in
  let line = max 1 i in
  let bol = line_starts.(i) in
  let col = offset - bol in
  (line, col)

let edits ~(old_text : string) ~(new_text : string) : Text_edit.t list option =
  let a = split_lines_keep_ends old_text in
  let b = split_lines_keep_ends new_text in
  let hs = myers_hunks a b |> coalesce in

  (* Heuristics: if too many hunks, bail out. *)
  if List.length hs > 200 then None
  else
    let starts = line_starts a in
    let mk_replacement h =
      let buf = Buffer.create 128 in
      for j = h.b_start to h.b_start + h.b_len - 1 do
        Buffer.add_string buf b.(j)
      done;
      Buffer.contents buf
    in
    let to_edit h =
      let start_off = starts.(h.a_start) in
      let end_off = starts.(h.a_start + h.a_len) in
      let l1, c1 = pos_of_offset ~line_starts:starts ~offset:start_off in
      let l2, c2 = pos_of_offset ~line_starts:starts ~offset:end_off in
      {
        Text_edit.range =
          { start_ = { line = l1; col = c1 }; end_ = { line = l2; col = c2 } };
        replacement = mk_replacement h;
      }
    in
    let es = List.map to_edit hs in
    (* Ensure descending by start offset. Since hunks are in forward order, reverse. *)
    Some (List.rev es)

let show_hunks hs =
  hs
  |> List.map (fun h ->
      Printf.sprintf "{a_start=%d;a_len=%d;b_start=%d;b_len=%d}" h.a_start
        h.a_len h.b_start h.b_len)
  |> String.concat "; "

let%test_unit "myers_hunks: two separated substitutions yield two hunks" =
  let a = [| "a\n"; "b\n"; "c\n"; "d\n" |] in
  let b = [| "a\n"; "B\n"; "c\n"; "e\n" |] in
  let hs = myers_hunks a b |> coalesce in
  let expected =
    [
      { a_start = 1; a_len = 1; b_start = 1; b_len = 1 };
      { a_start = 3; a_len = 1; b_start = 3; b_len = 1 };
    ]
  in
  if hs <> expected then
    failwith
      (Printf.sprintf "Unexpected hunks.\nExpected: %s\nGot: %s\n"
         (show_hunks expected) (show_hunks hs))

let%test_unit "myers_hunks: insertion yields a_len=0" =
  let a = [| "a\n"; "c\n" |] in
  let b = [| "a\n"; "b\n"; "c\n" |] in
  let hs = myers_hunks a b |> coalesce in
  let expected = [ { a_start = 1; a_len = 0; b_start = 1; b_len = 1 } ] in
  if hs <> expected then
    failwith
      (Printf.sprintf "Unexpected hunks.\nExpected: %s\nGot: %s\n"
         (show_hunks expected) (show_hunks hs))
