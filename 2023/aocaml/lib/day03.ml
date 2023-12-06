(* add up all the numbers close (even diagonally) to a special character*)
(* Parse input and keep number / char with index of start/stop *)

let get_data f = In_channel.input_all (open_in f)

type token = Number of int * int | Symbol of int

(* let part_1 input =
   let queue = Queue.create () in

   String.split_on_char '\n' input
   |> List.hd
   |> String.iteri (fun i c ->
   Queue.add (Number (i, int_of_string (String.make 1 c))) queue);
   1 *)

let raaaaaaaa s =
  String.fold_left
    (fun (idx, lst) c ->
      ( succ 1,
        match c with
        | '0' .. '9' -> lst @ [ Number (idx, int_of_string (String.make 1 c)) ]
        | '.' -> lst
        | _ -> lst @ [ Symbol idx ] ))
    (0, []) s
  |> snd

(* let merge tlist = *)

let solve () = ()
(* let input_data = get_data "data/03_input.txt" in
   Printf.printf "[03] - Part 1: %d\n" @@ part_1 input_data; *)

(* let test = "..512..987." in
   test
   |> String.fold_left
       (fun (acc, buff) c ->
         match c with
          (* buff to acc *)
         | '.' ->
             ( (match buff with "" -> acc | _ -> acc @ [ int_of_string buff ]),
               "" )
         | '0' .. '9' -> (acc, buff ^ String.make 1 c)
         | _ -> failwith "not implemented")
       ([], "")
   |> fst |> List.iter (Printf.printf "%d \n") *)

(*
   - read line by line, char by char and store everything as a token list list
   - compact the token list list if index is close to make digits
   - for each digit, check if there are symbols in 8-voisinage
*)
