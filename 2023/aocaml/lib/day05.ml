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

let seed_to_location all_mappings seed =
  List.fold_left (fun base e -> find_dest_in_map e base) seed all_mappings

(* For each seed, find the location & min over the list of locations *)
let solve_part_1 input =
  let all_mappings, seeds = parse input in
  List.map (seed_to_location all_mappings) seeds
  |> List.fold_left min Int.max_int
let solve () =
  let input = Io.load_as_list "data/05_input.txt" in
  (* 111627841 *)
  Printf.printf "[05] - Part 1: %d\n" @@ solve_part_1 input;
  (* 69323689 & 475615938 are too high *)
  Printf.printf "[05] - Part 2: %d\n" @@ solve_part_2 input
