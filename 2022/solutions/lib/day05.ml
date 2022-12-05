let filename = "data/05_input"

let s1 =
  let stack = Stack.create () in
  Stack.push 'Z' stack;
  Stack.push 'T' stack;
  Stack.push 'F' stack;
  Stack.push 'R' stack;
  Stack.push 'W' stack;
  Stack.push 'J' stack;
  Stack.push 'G' stack;
  stack

let s2 =
  let stack = Stack.create () in
  Stack.push 'G' stack;
  Stack.push 'W' stack;
  Stack.push 'M' stack;
  stack

let s3 =
  let stack = Stack.create () in
  Stack.push 'J' stack;
  Stack.push 'N' stack;
  Stack.push 'H' stack;
  Stack.push 'G' stack;
  stack

let s4 =
  let stack = Stack.create () in
  Stack.push 'J' stack;
  Stack.push 'R' stack;
  Stack.push 'C' stack;
  Stack.push 'N' stack;
  Stack.push 'W' stack;
  stack

let s5 =
  let stack = Stack.create () in
  Stack.push 'W' stack;
  Stack.push 'F' stack;
  Stack.push 'S' stack;
  Stack.push 'B' stack;
  Stack.push 'G' stack;
  Stack.push 'Q' stack;
  Stack.push 'V' stack;
  Stack.push 'M' stack;
  stack

let s6 =
  let stack = Stack.create () in
  Stack.push 'S' stack;
  Stack.push 'R' stack;
  Stack.push 'T' stack;
  Stack.push 'D' stack;
  Stack.push 'V' stack;
  Stack.push 'W' stack;
  Stack.push 'C' stack;
  stack

let s7 =
  let stack = Stack.create () in
  Stack.push 'H' stack;
  Stack.push 'B' stack;
  Stack.push 'N' stack;
  Stack.push 'C' stack;
  Stack.push 'D' stack;
  Stack.push 'Z' stack;
  Stack.push 'G' stack;
  Stack.push 'V' stack;
  stack

let s8 =
  let stack = Stack.create () in
  Stack.push 'S' stack;
  Stack.push 'J' stack;
  Stack.push 'N' stack;
  Stack.push 'M' stack;
  Stack.push 'G' stack;
  Stack.push 'C' stack;
  stack

let s9 =
  let stack = Stack.create () in
  Stack.push 'G' stack;
  Stack.push 'P' stack;
  Stack.push 'N' stack;
  Stack.push 'W' stack;
  Stack.push 'C' stack;
  Stack.push 'J' stack;
  Stack.push 'D' stack;
  Stack.push 'L' stack;
  stack

let crate_array = [| s1; s2; s3; s4; s5; s6; s7; s8; s9 |]

let simulate () =
  let rec f amount src dest =
    match amount with
    | 0 -> () (* failwith "Nothing to move arround" *)
    | n -> (
        match Stack.pop_opt crate_array.(src - 1) with
        | None -> ()
        | Some e ->
            Stack.push e crate_array.(dest - 1);
            f (pred n) src dest)
  in

  let chan = open_in filename in
  let rec loop = function
    | i when i >= 10 -> (
        match In_channel.input_line chan with
        | None -> ()
        | Some line ->
            Scanf.sscanf line "move %d from %d to %d" (fun amount src dest ->
                f amount src dest);
            loop (succ i))
    | i ->
        let _aze = In_channel.input_line chan in
        loop (succ i)
  in
  loop 0

let run () =
  simulate ();
  let s = ref "" in
  for i = 0 to Array.length crate_array - 1 do
    let c =
      match Stack.top_opt crate_array.(i) with None -> ' ' | Some x -> x
    in
    s := String.concat "" [ !s; String.make 1 c ]
  done;
  Printf.printf "[05] - Part 1: %s\n" !s
