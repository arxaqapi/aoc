open Printf
open Scanf

let filename = "13_input"

let fold_data = [('x',655);('y',447);('x',327);('y',223);('x',163);('y',111);('x',81);('y',55);('x',40);('y',27);('y',13);('y',6)]


(* let foldx = 655 *)
let read () = 
  let chan = Scanning.open_in filename in
  let data = List.init 735 @@ (fun _ -> bscanf chan "%d,%d\n" (fun a b -> a, b))
  in Scanning.close_in chan;
  data


let rec add_unique e l = match l with
  | [] -> e :: []
  | hd :: tl -> if hd = e then hd :: tl else hd :: add_unique e tl

let solve bl ftuple = 
  let nl = ref [] in
  let rec trait l = match l with
  | [] -> !nl
  | (x, y) :: tl ->
    (match ftuple with
      | ('x', fvx) -> 
        if x > fvx
          then (nl := add_unique ((x - 2 * (x - fvx)), y) !nl; trait tl)
          else (nl := add_unique (x, y) !nl; trait tl)
      | ('y', fvy) -> 
        if y > fvy
          then (nl := add_unique (x, (y - 2 * (y - fvy))) !nl; trait tl)
          else (nl := add_unique (x, y) !nl; trait tl)
      | _, _ -> failwith "error while solving"
    )
  in trait bl


let print_sol l =
  let xmax = List.fold_left (fun bx (ex,ey) -> max bx ex) (fst (List.hd l)) (List.tl l) in
  let ymax = List.fold_left (fun by (ex,ey) -> max by ey) (snd (List.hd l)) (List.tl l) in
  for j = 0 to ymax do
    for i = 0 to xmax do
      if List.exists (fun e -> e = (i, j)) l 
        then printf "# "
        else printf ". "
    done;
    printf "\n";
  done

let () = 
  solve (read ()) (List.hd fold_data)
  |> List.length
  |> printf "day13.1: %i\n"; (* (602) *)
  
  printf "day13.2:\n";
  List.fold_left (fun b e -> solve b e) (read ()) fold_data
  |> print_sol; (* CAFJHZCK *)
  
  assert(add_unique 5980 [1;2;0;98] = [1;2;0;98;5980]);
  assert(add_unique 0 [1;2;0;98] = [1;2;0;98]);
