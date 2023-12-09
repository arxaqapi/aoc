let get_last l = List.rev l |> List.hd
let parse input = String.split_on_char ' ' input |> List.map int_of_string

let madness_descent l =
  let rec descent l =
    match List.for_all (( = ) 0) l with
    | true -> 0
    | false ->
        let res = Io.map_by_two (fun l r -> r - l) l in
        get_last res + descent res
  in
  descent l + get_last l

let solve_part_1 input =
  List.map parse input |> List.map madness_descent |> List.fold_left ( + ) 0

let solve_part_2 input =
  List.map parse input |> List.map List.rev |> List.map madness_descent
  |> List.fold_left ( + ) 0

let solve () =
  let input = Io.load_as_list "data/09_input.txt" in
  (*  *)
  Printf.printf "[09] - Part 1: %d\n" @@ solve_part_1 input;
  (*  *)
  Printf.printf "[09] - Part 2: %d\n\n" @@ solve_part_2 input
