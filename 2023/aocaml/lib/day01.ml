let input_data = "data/01_input.txt"

let is_number c =
  let ascii_of_c = Char.code c in
  ascii_of_c >= 48 && ascii_of_c <= 57

let get_number_from_char c = int_of_char c - int_of_char '0'

(* !! Can overflow! no safeguards *)
let rec search s i incr =
  match s.[i] with
  | c when is_number c -> get_number_from_char c
  | _c -> search s (i + incr) incr

let retrieve_calibration_value s =
  (search s 0 1, search s (String.length s - 1) (-1))

let part_1_content f =
  (* while not none, read input and retrieve calibration value *)
  let rec loop_input chan total =
    match In_channel.input_line chan with
    | None -> total
    | Some s ->
        let values = retrieve_calibration_value s in
        loop_input chan (total + ((fst values * 10) + snd values))
  in
  loop_input (open_in f) 0

(* Part 2 *)
let patterns =
  [ "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" ]

(* overlap is not taken into account *)
let string_to_int s =
  match s with
  | "one" -> 1
  | "two" -> 2
  | "three" -> 3
  | "four" -> 4
  | "five" -> 5
  | "six" -> 6
  | "seven" -> 7
  | "eight" -> 8
  | "nine" -> 9
  | _ -> failwith "not a decimal"

let check_pattern s i pattern =
  (* check that pattern is shorter or equal to string lenght and current position *)
  match String.length pattern + i <= String.length s with
  | false -> None
  | true -> (
      match pattern = String.sub s i (String.length pattern) with
      | true -> Some (string_to_int pattern)
      | false -> None)

let check_all_patterns s i =
  let rec loop pattern =
    match pattern with
    | [] -> None
    | hd :: tl -> (
        match check_pattern s i hd with None -> loop tl | Some e -> Some e)
  in
  loop patterns

let string_to_calibration_v s =
  let i_list =
    String.to_seq s |> List.of_seq
    |> List.fold_left2
         (fun b i c ->
           match is_number c with
           | true -> get_number_from_char c :: b
           | false -> (
               match check_all_patterns s i with None -> b | Some x -> x :: b))
         []
         (List.init (String.length s) (fun i -> i))
  in
  (10 * (i_list |> List.rev |> List.hd)) + List.hd i_list

let part_2 f =
  let rec loop_input chan total =
    match In_channel.input_line chan with
    | None -> total
    | Some s -> loop_input chan (total + string_to_calibration_v s)
  in
  loop_input (open_in f) 0

let solve () =
  (* 55386 , 54824 *)
  Printf.printf "[01] - Part 1: %d\n" @@ part_1_content input_data;
  Printf.printf "[01] - Part 2: %d\n\n" @@ part_2 input_data
