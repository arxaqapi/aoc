

let filename = "10_input"


let data file = 
  let chan = Scanf.Scanning.open_in file in
  let try_read_line () = 
    try Some (Scanf.bscanf chan "%s\n" (fun l -> l))
    with End_of_file -> None
  in
  let rec loop_read lst = match try_read_line () with
    | Some (l) -> loop_read @@ l :: lst
    | None -> Scanf.Scanning.close_in chan; List.rev lst
  in loop_read [] |> Array.of_list



(* let s = Stack.create *)

let () = 
let lines = data filename in
 print_endline lines.(Array.length lines - 1)
 (* Printf.printf "lenght: %d" @@ Array.length lines *)