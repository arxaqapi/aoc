
let input_data = "input_01.txt"
(* 
- each line is a calibration value to retrieve
- the value is the first encountered digit concatenated to the last digit
*)

let is_number c = 
  let ascii_of_c = Char.code c in 
  ascii_of_c >= 48 && ascii_of_c <= 57

let get_number_from_char c = int_of_char c - int_of_char '0'

  (* !! Can overflow! no safeguards *)
let rec search s i incr = match s.[i] with
  | c when is_number c -> get_number_from_char c
  | c -> search s (i + incr) incr

let retrieve_calibration_value s =
  search s 0 1, search s (String.length s - 1) (- 1)


let part_1_content f =
  (* while not none, read input and retrieve calibration value *)
  let rec loop_input chan total = match In_channel.input_line chan with
    | None -> total
    | Some s -> 
      let values = retrieve_calibration_value s in
      Printf.printf "values: %d %d\n" (fst values) (snd values) ;
      loop_input chan (total + (fst values * 10 + snd values))
  in loop_input (open_in f) 0


(* Part 2 *)
let patterns = [|
  "one"
  ;"two"
  ;"three"
  ;"four"
  ;"five"
  ;"six"
  ;"seven"
  ;"eight"
  ;"nine"
  |]

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
  match pattern == String.sub s i (String.length pattern) with
  | true -> Some (string_to_int pattern)
  | false -> None

let check_all_patterns s i = check_pattern s i patterns.(0)

(* for each i in string: 
    if decimal, add to queue and i + 1
    else if is pattern, get int from string and move i + 2
*)
(* let aaaaaaa s = 
  let q = Queue.create () in
  String.iteri (fun i c -> match is_number c with
    | true -> Queue.push (get_number_from_char c) q
    | false -> let _ = check_all_patterns s i in ()
  ) s;
  Queue.peek + Queue. *)


let () = 
  (* 55386 , 54824 *)
  Printf.printf "[01] part 1: %d | part 2: %d\n" (part_1_content input_data) 0 