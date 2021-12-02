
let filename = "02_input"


let read_file f = 
  let channel = Scanf.Scanning.from_file f in
  let rec try_read_line () =
    try Some (Scanf.bscanf channel "%s %d\n" (fun i v -> i, v))
    with End_of_file -> None
  in
  let rec loop list = match try_read_line () with
    | Some (v) -> loop (v :: list)
    | None -> Scanf.Scanning.close_in channel; List.rev list in
  loop []


let compute l = 
  let horizontal = ref 0 in
  let depth = ref 0 in
  let rec f l = match l with
    | h :: tl -> (match h with
      | "forward", v -> horizontal := !horizontal + v 
      | "down", v -> depth := !depth + v
      | "up", v -> depth := !depth - v
      | _, _ -> failwith "Error"); f tl
    | _ -> !horizontal * !depth
  in
  f l

let day2 l = 
  let horizontal = ref 0 in
  let depth = ref 0 in
  let aim = ref 0 in
  let rec f l = match l with
    | h :: tl -> (match h with
      | "forward", v -> 
        horizontal := !horizontal + v;
        depth := !depth + !aim * v
      | "down", v -> aim := !aim + v
      | "up", v -> aim := !aim - v
      | _, _ -> failwith "Error"); f tl
    | _ -> !horizontal * !depth
  in
  f l


let () =
  let data = read_file filename in
  Printf.printf "day2.1: %d\nday2.2: %d\n" (compute data) (day2 data)