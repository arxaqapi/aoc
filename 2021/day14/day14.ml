
(* let filename = "14_input" *)
let filename = "example"

(* let *)

let read_file =
  let chan = open_in filename in
  really_input_string chan (in_channel_length chan)
  |> String.split_on_char '\n'
  |> List.filter (fun e -> String.length e > 0 )

let splitter data = 
  let str = List.hd data in
  let rules = List.tl data in
  let table = Hashtbl.create 50 in
  let rec rules_creator rules = match rules with
    | [] -> table
    | hd :: tl ->
      Hashtbl.add table (String.sub hd 0 2) (String.make 1 (hd.[6]));
      rules_creator tl
  in str, rules_creator rules

let w = Printf.sprintf "%c%c" 'a' 'b'

let expand data rule_table = 
  let ldata = String.to_seq data
  |> List.of_seq in
  let rec trait l = match l with
    | h1 :: h2 :: tl ->
      let new_s = (Hashtbl.find rule_table (String.of_seq @@ List.to_seq [h1;h2])) in
      (String.make 1 h1)
      ^ new_s
      (* ^ (String.make 1 h2) *)
      ^ trait (h2 :: tl)
    | h2 :: [] -> String.make 1 h2
    | _ -> ""
  in trait ldata

let solve runs data rule_table = 
  let rec trait i str = match i with
    | 0 -> str
    | _ -> trait (i - 1) (expand str rule_table)
  in trait runs data 


let () =
  let str, rule_table = splitter read_file in
  solve 3 str rule_table
  |> print_endline

(* start with 10 steps *)
(* find most common and least common element *)
(* count_most_common - count_least_common *)