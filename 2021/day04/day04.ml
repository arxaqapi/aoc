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
  Array.iter (fun ia -> Array.iter (Printf.printf "%d ") ia;print_newline ()) b


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
  let a = Array.init 5 (fun i -> line () |> Array.of_list) in
  flush ();
  a

let get_all_boards () =
  let counter = ref 0 in
  let try_get_board () = 
    try Some(get_board ())
    with End_of_file -> None in
  while true do
    match try_get_board () with
    | Some(b) -> Printf.printf "Got board ! > "; Printf.printf "%d\n" b.(0).(0); counter := !counter + 1
    | None -> Printf.printf "End of file reached ! Got %d boards" !counter; exit 1
  done

let () = 
  let d = draws in
  flush () ;
  get_all_boards ();

  let b = get_board () in print_board b