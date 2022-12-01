let filename = "01_input"

(*
The Elves take turns writing down the number of Calories contained by the various meals,...
that they've brought with them, one item per line.
Each Elf separates their own inventory from the previous Elf's inventory (if any) by a blank line.   
*)
let array_maxi a =
  let max_e = ref @@ a.(0) in
  let max_i = ref 0 in
  for i = 0 to Array.length a - 1 do
    if a.(i) > !max_e 
      then
        max_e := a.(i);
        max_i := i
  done;
  !max_i, !max_e

let read_all_input f = 
  let chan = open_in f in
  let cal_per_elve = Queue.create () in

  let rec loop acc = match In_channel.input_line chan with
    | None -> ()
    | Some "" -> Queue.push acc cal_per_elve; loop 0
    | Some x -> loop (acc + int_of_string x)
  in loop 0;
  Queue.to_seq cal_per_elve
  |> Array.of_seq
  |> array_maxi
  

let part_two f =
  let chan = open_in f in
  let cal_per_elve = Queue.create () in
  let rec loop acc = match In_channel.input_line chan with
    | None -> ()
    | Some "" -> Queue.push acc cal_per_elve; loop 0
    | Some x -> loop (acc + int_of_string x)
  in loop 0;
  let calories = Queue.to_seq cal_per_elve
  |> Array.of_seq in
  Array.sort compare calories;
  let len = Array.length calories - 1 in
  calories.(len - 2) + calories.(len - 1) + calories.(len)
  



let () = 
  let maxi, maxe = read_all_input filename in
  (* Elve n°: 254 with 66487 calories*)
  Printf.printf "Solution: Elve n°: %d with %d calories\n" (maxi + 1) maxe;
  (* 197301 *)
  Printf.printf "Solution Part 2: %d\n" @@ part_two filename