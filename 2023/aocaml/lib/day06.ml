(* Go farther in earch race than record
     - holding down charges boat
     - releasing allows boat to move
     - boat moves faster if button held down for long

   Result is the product of the sum of number of times the record can be broken
*)

(* takes the [speed] and [time_left] into account to calculate how far the boat can go *)
let distance_traveled speed time_left = speed * time_left

let n_record_beating_times time distance_record =
  let hold_down_times = List.init (pred time) succ in
  List.fold_left
    (fun acc e ->
      match distance_traveled e (time - e) > distance_record with
      (* if better than record, add 1 to acc *)
      | true -> succ acc
      | _ -> acc)
    0 hold_down_times

let get_beating_record_prod input_tuple =
  List.fold_left
    (fun acc (time, dist_rec) ->
      match n_record_beating_times time dist_rec with
      | n when n > 0 -> acc * n
      | _ -> acc)
    1 input_tuple

let solve_part_1 _input =
  get_beating_record_prod [ (44, 277); (89, 1136); (96, 1890); (91, 1768) ]

let solve_part_2 _input =
  get_beating_record_prod [ (44899691, 277113618901768) ]

let solve () =
  let input = Io.load_as_list "data/06_input.txt" in
  (* 2344708 *)
  Printf.printf "[06] - Part 1: %d\n" @@ solve_part_1 input;
  (* 30125202 *)
  Printf.printf "[06] - Part 2: %d\n" @@ solve_part_2 input
