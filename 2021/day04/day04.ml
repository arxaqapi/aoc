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
  let try_get_board () = 
    try Some(get_board ())
    with End_of_file -> None in
  Array.init 100 (fun i -> match try_get_board () with Some(b)->b | None -> failwith "error" )


let () = 
  let d = draws in
  flush () ;
  (get_all_boards ()).(99)
  |> print_board
