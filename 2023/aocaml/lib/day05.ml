(* true_dest = s + offset + i *)
type mapping = { start : int; length : int; dest_offset : int }
type map = mapping list

let parse_mapping_line line =
  let [ dest; src; length ] =
    String.split_on_char ' ' line |> List.map int_of_string
  in
  { start = src; length; dest_offset = dest - src }

(* FIXME: fix empty first list if first string is sep *)
let rec split_on_first_sep sep l =
  match l with
  | hd :: tl when hd <> sep ->
      let acc, rest = split_on_first_sep sep tl in
      (hd :: acc, rest)
  | hd :: tl when hd = sep -> ([], tl)
  | _ -> ([], [])

let rec split_on_sep sep l =
  match split_on_first_sep sep l with
  | [], [] -> []
  | head, [] -> head :: []
  | head, after_sep -> head :: split_on_sep sep after_sep

let parse input =
  let seeds = List.hd input in
  let seeds =
    String.sub seeds 7 (String.length seeds - 7)
    |> String.split_on_char ' ' |> List.map int_of_string
  in
  ( List.tl input |> split_on_sep ""
    |> List.filter (fun l -> List.length l > 0)
    |> List.map List.tl
    |> List.map (List.map parse_mapping_line),
    seeds )

let find_dest_in_map (mappings : map) input =
  let rec loop mappings =
    match mappings with
    | { start; length; dest_offset } :: _
      when input >= start && input <= start + length ->
        input + dest_offset
    | _ :: tl -> loop tl
    | [] -> input
  in
  loop mappings

(* Takes [all_mappings] and reduces it to the location value from the initial [seed].  *)
let seed_to_location all_mappings seed =
  List.fold_left (fun base e -> find_dest_in_map e base) seed all_mappings

(* For each seed, find the location & min over the list of locations *)
let solve_part_1 input =
  let all_mappings, seeds = parse input in
  List.map (seed_to_location all_mappings) seeds
  |> List.fold_left min Int.max_int

(* Applies [f_map] on each value in the range of [start] + [len] and folds/reduces with [f]. *)
let fold_left_map_range f accu f_map (start, len) =
  let rec loop acc i =
    match i with
    | i when i < len -> loop (f acc (f_map (start + i))) (succ i)
    | _ -> acc
  in
  loop accu 0

let reduce_map_range all_mappings (start, len) =
  let min_loc = ref Int.max_int in
  for i = 0 to len - 1 do
    (* start + i *)
    min_loc := min !min_loc (seed_to_location all_mappings (start + i))
  done;
  !min_loc

(* let seed_range_list (start, length) = List.init length (fun i -> start + i) *)

let solve_part_2 input =
  let all_mappings, _seeds = parse input in
  let test =
    [
      (3136945476, 509728956);
      (1904897211, 495273540);
      (1186343315, 66026055);
      (1381149926, 11379441);
      (4060485949, 190301545);
      (444541979, 351779229);
      (1076140984, 104902451);
      (264807001, 60556152);
      (3676523418, 44140882);
      (3895155702, 111080695);
    ]
  in

  (* Iterative solution w. stack explosion *)
  (* List.fold_left
     (fun acc e ->
       min acc
         (List.map (seed_to_location all_mappings) (seed_range_list e)
         |> List.fold_left min Int.max_int))
     Int.max_int test *)

  (* Recursive solution w. handcrafted fold *)
  List.map
    (fold_left_map_range min Int.max_int (seed_to_location all_mappings))
    test
  |> List.fold_left min Int.max_int

(* for loop solution with index *)
(* List.map (reduce_map_range all_mappings) test
   |> List.fold_left min Int.max_int *)

let solve () =
  let input = Io.load_as_list "data/05_input.txt" in
  (* 111627841 *)
  Printf.printf "[05] - Part 1: %d\n" @@ solve_part_1 input;
  (* 69323688 *)
  Printf.printf "[05] - Part 2: %d\n\n" @@ solve_part_2 input
