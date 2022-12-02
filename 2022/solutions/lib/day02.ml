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



  
let follow_strategy () =
  (* 10624 *)
  Printf.printf "Total score with naive strategy= %d\n" @@ read_sum filename
