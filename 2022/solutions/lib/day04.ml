let filename = "data/04_input"

(*
each line, pair of sections ids   
*)

(*
2.4 -> 234
3.3 -> 3   
*)

let is_contained l =
  match l with
  | [ a; b; c; d ] -> (a <= c && d <= b) || (c <= a && b <= d)
  | _ -> failwith "what?"

let get_lines_n_sum f =
  let chan = open_in f in
  let rec loop s =
    match In_channel.input_line chan with
    | None -> s
    | Some l -> (
        match
          String.split_on_char ',' l
          |> List.map (String.split_on_char '-')
          |> List.flatten |> List.map int_of_string |> is_contained
        with
        | true -> loop (succ s)
        | false -> loop s)
  in
  let res = loop 0 in
  close_in chan;
  res

let run () = Printf.printf "[04] - Part 1: %d\n" @@ get_lines_n_sum filename
