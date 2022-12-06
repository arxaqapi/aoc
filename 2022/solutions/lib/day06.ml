let filename = "data/06_input"

(*
Look for a start of packet
  -> 4 characters that are all different    
*)
let count_s s e = String.fold_left (fun b c -> if c = e then succ b else b) 0 s
let count_l l e = List.fold_left (fun b c -> if c = e then succ b else b) 0 l

let traverse s =
  let get = String.get s in
  let rec loop i c =
    match (i, c) with
    | i, _ when i = String.length s -> i
    | _, false -> i
    | i, _ ->
        let part = [ get (i - 3); get (i - 2); get (i - 1); get i ] in
        let count_l = List.map (fun e -> count_l part e) part in
        if List.fold_left ( + ) 0 count_l = 4 then loop (succ i) false
        else loop (succ i) true
  in
  loop 3 true

let gen_traverse s size =
  let get = String.get s in
  let rec loop i c =
    match (i, c) with
    | i, _ when i = String.length s -> i
    | _, false -> i
    | i, _ ->
        (* get a list of the last (size) elements *)
        let part = List.init size (fun ii -> get (i - ii)) in
        let count_l = List.map (fun e -> count_l part e) part in
        if List.fold_left ( + ) 0 count_l = size then loop (succ i) false
        else loop (succ i) true
  in
  loop (size - 1) true

let run () =
  let s =
    match In_channel.input_line (open_in filename) with
    | Some s -> s
    | None -> failwith "Empty file"
  in

  (* 1109 *)
  Printf.printf "[06] - Part 1: %d\n" @@ traverse s;
  (* 3965 *)
  Printf.printf "[06] - Part 2: %d\n" @@ gen_traverse s 14
