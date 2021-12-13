open Printf
open Scanf

let filename = "13_input"

let foldx = 655
let read = 
  let chan = Scanning.open_in filename in
  List.init 735 @@ (fun _ -> bscanf chan "%d,%d\n" (fun a b -> a, b))


let rec add_unique e l = match l with
  | [] -> e :: []
  | hd :: tl -> if hd = e then hd :: tl else hd :: add_unique e tl

let solve bl = 
  let nl = ref [] in
  let rec trait l = match l with
  | [] -> !nl
  | (x, y) :: tl ->
    if x > foldx 
      then (nl := add_unique ((x - 2 * (x - foldx)), y) !nl; trait tl)
      else (nl := add_unique (x, y) !nl; trait tl)
  in trait bl

let () = 
  assert(add_unique 5980 [1;2;0;98] = [1;2;0;98;5980]);
  assert(add_unique 0 [1;2;0;98] = [1;2;0;98]);
  printf "day13: %i" @@ List.length (solve read)
