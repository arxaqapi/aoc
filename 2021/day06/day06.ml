

let filename = "06_input"

let print_list l =
  List.iter (Printf.printf "%i ") l; print_newline ()

let lf = 
  open_in filename
  |> input_line
  |> String.split_on_char ','
  |> List.map int_of_string

let rec bloop lf = match lf with
  | [] -> lf
  | hd :: tl -> 
    if hd = 0 
      then 6 :: (bloop tl) @ (8 :: [])
      else hd - 1 :: (bloop tl)  

let rec simulate days lf = match days with
  | 0 -> List.length lf
  | n -> 
    let nlf = bloop lf in 
    (* print_list nlf; *)
    simulate (n - 1) nlf

let () =
  Printf.printf "day06.1: %i\n" @@ simulate 40 lf 

  