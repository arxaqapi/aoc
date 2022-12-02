let filename = "data/02_input"

let get_score_move = function
  | "A" | "X" -> 1
  | "B" | "Y" -> 2
  | "C" | "Z" -> 3
  | _ -> failwith "Move is not correct"

let get_score_round opponent_move move =
  match (opponent_move, move) with
  | xyz, abc when get_score_move xyz = get_score_move abc ->
      get_score_move move + 3 (* Draw *)
  | "A", "Y" | "B", "Z" | "C", "X" -> get_score_move move + 6 (* Won *)
  | _, _ -> get_score_move move + 0 (* Lost *)

let get_score_round_part_2 opponent_move move =
  (* X means you need to lose, Y means you need to end the round in a draw, and Z means you need to win.*)
  match (opponent_move, move) with
  | o, "X" -> (
      match get_score_move o + 2 with 3 -> 3 | x -> x mod 3 (* Need to lose *))
  | o, "Y" -> get_score_move o + 3 (* Need to draw *)
  | o, "Z" ->
      (match get_score_move o + 1 with 3 -> 3 | x -> x mod 3)
      + 6 (* Need to win *)
  | _, _ -> failwith "Not implemented"

let read_sum f =
  let chan = open_in f in
  let rec loop_read sum =
    match In_channel.input_line chan with
    | None -> sum
    | Some s ->
        let line = String.split_on_char ' ' s in
        loop_read
          (sum + get_score_round (List.hd line) (List.hd @@ List.tl line))
  in
  loop_read 0

let read_sum_part_2 f =
  let chan = open_in f in
  let rec loop_read sum =
    match In_channel.input_line chan with
    | None -> sum
    | Some s ->
        let line = String.split_on_char ' ' s in
        loop_read
          (sum + get_score_round_part_2 (List.hd line) (List.hd @@ List.tl line))
  in
  loop_read 0

let follow_strategy () =
  (* 10624 *)
  Printf.printf "Total score with naive strategy= %d\n" @@ read_sum filename
  (* 14060 *);
  Printf.printf "Total score part 2: %d\n" @@ read_sum_part_2 filename
