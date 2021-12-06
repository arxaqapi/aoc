(*
get all draws
get all boards
compute winning board
*)
let file = "04_input"

let chan = open_in file
(* close_in *)

let flush () = let _ = input_line chan in ()

let print_board b = 
  Array.iter (fun ia -> Array.iter (fun e -> Printf.printf "%d " @@ snd e) ia;print_newline ()) b

  let draws =
  input_line chan
  |> String.split_on_char ','
  |> List.map int_of_string

(* For each board, init array with read line *)
let get_board () =
  let line () = 
    input_line chan 
    |> String.split_on_char ' ' 
    |> List.filter (fun e -> String.length e > 0)
    |> List.map int_of_string 
  in
  let a = Array.init 5 (fun i -> line () |> List.map (fun e -> (false, e) )  |> Array.of_list) in
  flush ();
  a

let get_all_boards () =
  let try_get_board () = 
    try Some(get_board ())
    with End_of_file -> None in
  Array.init 100 (fun i -> match try_get_board () with Some(b)->b | None -> failwith "error" )

(*
while no board finished, for each draw in draws:
  remove draw in each board
*)
let is_finished board = 
  (* A faire -> lignes [x] colonnes [ ] *)
  let l = Array.fold_left 
  ( fun b e -> 
    let line_res = 
      Array.fold_left (fun b e -> ((fst e) && (fst b), (snd e) + (snd b)))
      (true, 0)
      e (* la ligne en question *)
    in ((fst line_res) && (fst b), if fst b && fst line_res && snd b <> 0 then snd b else snd line_res)
  )
  (true, 0)
  board (* le tableau *) in
  (* let i = ref 0 in *)
  for j = 0 to 4 do
    for i = 0 to 4 do
      let b, v = board.(i).(j);
    done;
  done;
(*
iterate on each line and stamp -> bool value to true
*)
let stamp board draw = 
  for i = 0 to 4 do
    for j = 0 to 4 do
      match board.(i).(j) with
      | b, v -> if v = draw then board.(i).(j) <- (true, v)
    done
  done

let loop boards draws =
  (* Printf.printf "i: %i" i; *)
  (* pour chaque draw 
      check pour chaque boards si is_finished, si oui renvoie val et indice tableau*)
  let rec iloop draws = match draws with
    | draw :: tl -> 
      let rec bloop i = match i with
        | 100 -> failwith "je suis con"
        | i ->
          Printf.printf "i: %i\n" i;
          (* stamp all *)
           stamp boards.(i) draw;
           assert (i < 100);
           let b,v = is_finished boards.(i) in
           if b then v else bloop (i + 1)
      in bloop 0
      (* check if is_finished *)
    | _ -> failwith "Pas de board trouvé"
  in iloop draws
  (* let i = ref 0 in
  let res = ref (is_finished boards.(!i)) in
  while not @@ fst !res do
    stamp boards.(!i);
    res := (is_finished boards.(!i));
  done *)
  (* for i = 0 to List.length draws do
  done  *)

let () = 
  let d = draws in
  flush () ;
  loop (get_all_boards ()) d
  |> Printf.printf "day 04.1: %d"