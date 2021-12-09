

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
    in (trait [@tailcall]) lf [] [] 

let rec simulate days lf = match days with
  | 0 -> List.length lf
  | n -> 
    let nlf = bloop lf in 
    (* print_list nlf; *)
    simulate (n - 1) nlf

let rec fill lf arr = match lf with
    | [] -> arr
    | hd :: tl -> (arr.(hd) <- arr.(hd) + 1); fill tl arr

    let step arr =
  let newborns = arr.(0) in
  for i = 0 to 7 do
    if i = 6 
      then arr.(i) <- newborns + arr.(i + 1)
      else arr.(i) <- arr.(i + 1)
  done;
  arr.(8) <- newborns

let smarter lf days = 
  let days_left = fill lf (Array.make 9 0) in
  for i = 0 to days - 1 do
    step days_left;
  done;
  Array.fold_left (+) 0 days_left

let () =
  smarter lf 80
  |> Printf.printf "faster day6.2: %i\n";
  let total = simulate 80 lf in
  Printf.printf "day06.1: %i\n" total;
  assert (total = 391671); 

  