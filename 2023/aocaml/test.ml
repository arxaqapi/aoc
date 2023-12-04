let s = ("1234" 10 * 2) + (((10 * 3) + 4) 10 * (10 * 3) * 2)

let to_int s =
  let rec loop l acc = match l with [] -> acc | hd :: tl -> loop tl acc in
  loop (String.to_seq s |> List.of_seq) 0
