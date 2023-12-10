let parse_line line = 
  let key = String.sub line 0 3 in
  let left = String.sub line 7 3 in
  let right = String.sub line 12 3 in
  (key, left, right)

let parse input = 
  let instruction_list = List.hd input in
  let table = Hashtbl.create 700 in
  List.iter
    (fun line ->
    let key, left, right = parse_line line in
      Hashtbl.add table key (left, right))
    (List.tl @@ List.tl input);
  (instruction_list, table)

let infinite_loop instructions table = 
  let rec loop seq current_key =
    match seq () with
    | Seq.Nil -> failwith "Empty Sequence"
    | Seq.Cons (x, xs) -> (
    match Hashtbl.find table current_key with
      | _ when current_key = "ZZZ" -> 0
        | left_key, right_key -> (
            1
            + match x with 'L' -> loop xs left_key | 'R' -> loop xs right_key))
  in
  loop (Seq.cycle instructions) "AAA"

let end_with_a_keys table =
  Hashtbl.fold
    (fun key _ acc ->
      if String.ends_with ~suffix:"A" key then key :: acc else acc)
    table []
let infinite_loop_p2 instructions table =
  let rec loop seq current_keys =
    match seq () with
    | Seq.Nil -> failwith "Empty Sequence"
    | Seq.Cons (x, xs) -> (
        (* Return list of next keys using l or r *)
        let new_keys =
          List.map
            (fun current_key ->
              match Hashtbl.find table current_key with
              | left_key, right_key -> (
                  match x with 'L' -> left_key | 'R' -> right_key))
            current_keys
        in
        match
          List.fold_left
            (fun acc e -> acc && String.ends_with ~suffix:"Z" e)
            true new_keys
        with
        | true -> 0
        | false -> 1 + loop xs new_keys)
  in
  loop (Seq.cycle instructions) (end_with_a_keys table)

let solve_part_1 input = 
  let instruction_list, table = parse input in
  infinite_loop (String.to_seq instruction_list) table

let solve_part_2 input =
  let instruction_list, table = parse input in
  infinite_loop_p2 (String.to_seq instruction_list) table

let solve () =
  let input = Io.load_as_list "data/08_input.txt" in
  (* 22199 *)
  Printf.printf "[08] - Part 1: %d\n" @@ solve_part_1 input;
  (*  *)
  Printf.printf "[08] - Part 2: %d\n\n" @@ solve_part_2 input
