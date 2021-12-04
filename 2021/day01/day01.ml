
let filename = "01_input"


let read_file f = 
  let channel = Scanf.Scanning.from_file f in
  let rec try_read_line () =
    try Some (Scanf.bscanf channel "%d\n" (fun v -> v))
    with End_of_file -> None
  in
  let rec loop list = match try_read_line () with
    | Some (v) -> loop (v :: list)
    | None -> Scanf.Scanning.close_in channel; List.rev list in
  loop []

let read_file_as_string f = 
  let chan = open_in f in
  let measurements = really_input_string chan (in_channel_length chan)
  |> String.split_on_char '\n'
  |> List.filter (fun e -> String.length e > 0)
  |> List.map int_of_string in
  close_in chan;
  measurements

let rec sliding_sum l acc = match l with
  | a :: b :: c :: tl -> sliding_sum (b::c::tl) ((a + b + c) :: acc)
  | _ -> List.rev acc

let count_increase l = (match l with 
  | h :: tl -> 
    List.fold_left 
    (fun (c, b) e -> if e > b then (c + 1, e) else (c, e))
    (0, h)
    tl
  | _ -> failwith "empty")
  |> fst

let print_list l = List.iter (Printf.printf "%d, ") l 

let () =
let measurements = read_file_as_string filename in
  let day_1_1 = count_increase measurements in
  let day_1_2 = sliding_sum measurements [] |> count_increase in
  Printf.printf "Day 1.1: %d  Day 1.2: %d\n" day_1_1 day_1_2
