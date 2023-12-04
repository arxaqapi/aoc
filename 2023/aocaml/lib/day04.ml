type card = { id : int; magic_numbers : int list; numbers : int list }

let parse_input_line line =
  let card_num, rest =
    String.split_on_char ':' line |> function
    | [ hd; tl ] ->
        ( (String.split_on_char ' ' hd |> List.filter (( <> ) String.empty)
           |> function
           | [ _; tl ] -> int_of_string tl
           | _ -> failwith "could not get card n°"),
          tl )
    | _ -> failwith "error on string split on ':'"
  in
  let [ magic_numbers; numbers ] =
    String.split_on_char '|' rest
    |> List.map (fun s ->
           String.split_on_char ' ' s
           |> List.filter_map (fun s ->
                  match s <> String.empty with
                  | true -> Some (int_of_string s)
                  | false -> None))
  in
  { id = card_num; magic_numbers; numbers }

(* [count_n_winning number magic_number] returns all [magic_numbers] in [numbers] *)
let get_winning_n magic_numbers numbers =
  List.filter
    (fun n ->
      match List.find_opt (( = ) n) magic_numbers with
      | Some _ -> true
      | None -> false)
    numbers

let count_points winning_numbers =
  int_of_float (2. ** (float_of_int (List.length winning_numbers) -. 1.))

let solve_part_1 input =
  List.fold_left
    (fun acc e ->
      let card = parse_input_line e in
      get_winning_n card.magic_numbers card.numbers |> count_points |> ( + ) acc)
    0 input

let solve_part_2 input =
  (* make array of all cards *)
  let card_array = List.map parse_input_line input |> Array.of_list in
  let queue = Queue.create () in
  Array.iter (fun c -> Queue.add c queue) card_array;
  let rec loop count =
    match Queue.is_empty queue with
    | true -> count
    | false ->
        let card = Queue.pop queue in
        get_winning_n card.magic_numbers card.numbers
        |> List.iteri (fun i _ -> Queue.push card_array.(card.id + i) queue);
        loop (succ count)
  in
  loop 0

let solve () =
  let input = Io.load_as_list "data/04_input.txt" in
  (* 22193 *)
  Printf.printf "[04] - Part 1: %d\n" @@ solve_part_1 input;
  (* 5625994 *)
  Printf.printf "[04] - Part 2: %d\n" @@ solve_part_2 input
