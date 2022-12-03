let filename = "data/03_input"

open Utils

(*
1 line = 1 rucksack, 2 compartments ( 2n)   

Lowercase item types a through z have priorities 1 through 26.
Uppercase item types A through Z have priorities 27 through 52.
*)

let score_item s =
  match Char.code s with
  | c when c >= 97 && c <= 122 -> c - 96
  | c when c >= 65 && c <= 90 -> c - 38 (* 65 - 27 *)
  | _ -> failwith "Character out of range"

let incr_array_at array i = array.(i) <- array.(i) + 1

(* use array to store elements *)
let find_common_element s1 s2 =
  assert (String.length s1 = String.length s2);

  let mem_1 = Array.make (2 * 26) 0 in
  let mem_2 = Array.make (2 * 26) 0 in
  let rec loop i len =
    match i with
    | i when i = len -> () (* end of strings *)
    | i ->
        incr_array_at mem_1 (score_item s1.[i] - 1);
        (* index of the element in the mem_array *)
        incr_array_at mem_2 (score_item s2.[i] - 1);
        (* index of the element in the mem_array *)
        loop (succ i) len
  in
  loop 0 (String.length s1);
  let sum = ref 0 in
  Array.iter2i
    (fun i a b -> if a >= 1 && b >= 1 then sum := !sum + (i + 1))
    mem_1 mem_2;
  !sum

let read_n_sum f =
  let chan = open_in f in
  let rec loop_read sum =
    match In_channel.input_line chan with
    | None -> sum
    | Some s ->
        let len = String.length s in
        let s1 = String.sub s 0 (len / 2) in
        let s2 = String.sub s (len / 2) (len / 2) in
        loop_read (sum + find_common_element s1 s2)
  in
  let res = loop_read 0 in
  close_in chan;
  res

(*
3 lines = 1 group   
*)

let process_and_find_common l1 l2 l3 =
  let mem_1 = Array.make (2 * 26) 0 in
  let mem_2 = Array.make (2 * 26) 0 in
  let mem_3 = Array.make (2 * 26) 0 in
  String.iter (fun c -> incr_array_at mem_1 (score_item c - 1)) l1;
  String.iter (fun c -> incr_array_at mem_2 (score_item c - 1)) l2;
  String.iter (fun c -> incr_array_at mem_3 (score_item c - 1)) l3;
  let sum = ref 0 in
  Array.iter3i
    (fun i a b c -> if a >= 1 && b >= 1 && c >= 1 then sum := !sum + (i + 1))
    mem_1 mem_2 mem_3;
  !sum

let read_in_3 f =
  let chan = open_in f in
  let rec loop_read sum =
    match
      ( In_channel.input_line chan,
        In_channel.input_line chan,
        In_channel.input_line chan )
    with
    | None, _, _ -> sum
    | Some l1, Some l2, Some l3 ->
        assert (l1 <> l2 && l2 <> l3);
        loop_read (sum + process_and_find_common l1 l2 l3)
    | _, _, _ ->
        failwith "Input is not correctely formatted (there should be 3*n lines)"
  in
  loop_read 0

let run () =
  (* 7878 *)
  Printf.printf "Sum of the weights of the elements in the bckp: %d\n"
  @@ read_n_sum filename;
  Printf.printf "Part 2: %d\n" (read_in_3 filename)
