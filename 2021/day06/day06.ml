

let filename = "06_input"

let print_list l =
  List.iter (Printf.printf "%i ") l; print_newline ()

let lf = 
  open_in filename
  |> input_line
  |> String.split_on_char ','
  |> List.map int_of_string

let bloop lf =
  let rec trait lf acc =  match lf with
    | [] -> lf @ acc
    | hd :: tl -> 
      if hd = 0 
        then 6 :: (trait tl @@ 8 :: acc)
        else hd - 1 :: (trait tl acc)
  in trait lf [] 

let rec simulate days lf = match days with
  | 0 -> List.length lf
  | n -> 
    let nlf = bloop lf in 
    (* print_list nlf; *)
    simulate (n - 1) nlf

let () =
  Printf.printf "day06.1: %i\n" @@ simulate 40 lf 

  