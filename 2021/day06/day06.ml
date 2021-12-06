

let filename = "06_input"

let print_list l =
  List.iter (Printf.printf "%i ") l; print_newline ()

let lf = 
  open_in filename
  |> input_line
  |> String.split_on_char ','
  |> List.map int_of_string

let bloop lf =
    let rec trait lf nlf newborns =  match lf with
      | [] -> (List.rev nlf) @ lf @ newborns
      | hd :: tl -> 
        (match hd with
          | 0 -> (trait tl (6 :: nlf) (8 :: newborns))
          | n -> (trait tl (n - 1 :: nlf) newborns))
    in trait lf [] [] 

let rec simulate days lf = match days with
  | 0 -> List.length lf
  | n -> 
    let nlf = bloop lf in 
    (* print_list nlf; *)
    simulate (n - 1) nlf

let () =
  let total = simulate 80 lf in
  Printf.printf "day06.1: %i\n" total;
  assert (total = 391671); 

  