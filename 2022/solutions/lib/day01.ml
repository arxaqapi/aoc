let filename = "data/01_input"

let array_maxi a =
  let max_e = ref @@ a.(0) in
  let max_i = ref 0 in
  for i = 0 to Array.length a - 1 do
    if a.(i) > !max_e then max_e := a.(i);
    max_i := i
  done;
  (!max_i, !max_e)

let read_all_input f =
  let chan = open_in f in
  let cal_per_elve = Queue.create () in

  let rec loop acc =
    match In_channel.input_line chan with
    | None -> ()
    | Some "" ->
        Queue.push acc cal_per_elve;
        loop 0
    | Some x -> loop (acc + int_of_string x)
  in
  loop 0;
  Queue.to_seq cal_per_elve |> Array.of_seq |> array_maxi

let top_three_elves f =
  let chan = open_in f in
  let sum_list =
    In_channel.input_all chan
    |> Str.split (Str.regexp "\n\n")
    |> List.map (fun s ->
           String.split_on_char '\n' s
           |> List.filter (fun s -> s <> "")
           |> List.map int_of_string |> List.fold_left ( + ) 0)
  in
  List.sort compare sum_list |> List.rev |> function
  | a :: b :: c :: _tl -> a + b + c
  | _ -> 0

let run () =
  let maxi, maxe = read_all_input filename in
  (* Elve n°: 254 with 66487 calories*)
  Printf.printf "Solution: Elve n°: %d with %d calories\n" (maxi + 1) maxe;
  (* 197301 *)
  Printf.printf "Solution Part 2: %d\n" @@ top_three_elves filename
