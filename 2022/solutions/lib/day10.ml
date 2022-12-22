let filename = "data/10_input"

(*
X = 1   

addx V  -> takes 2 cycles to increase x by V
noop    -> takes 1 cycle to complete, does nothing

at Yth cycle => Y * X
*)

let key_cycles = [ 20; 60; 100; 140; 180; 220 ]

(*
get line
  check if noop or addx at beginninig
    increse current_cycle and X if addx
    ! while checking for key_cycles
*)

module Register = struct
  type t = { mutable x : int; mutable total : int; mutable current_cycle : int }

  let do_overlap r = 
    let cc = r.current_cycle mod 40 in
    (cc >= pred r.x) && (cc <= succ r.x)

  (** Make a cycle step *)
  let step r =
    (match r.current_cycle with
    | 20 | 60 | 100 | 140 | 180 | 220 ->
        r.total <- r.total + (r.current_cycle * r.x)
    | _ -> ());
    r.current_cycle <- r.current_cycle + 1

  let step_v2 r =
    let s = if do_overlap r then "#" else "." in
    Printf.printf "%s" s;
    if r.current_cycle mod 40 = 0 then print_newline ();
    r.current_cycle <- r.current_cycle + 1

  let add r v = r.x <- r.x + v
  let create () = { x = 1; total = 0; current_cycle = 1 }
end

let parse_line r s =
  match String.length s with
  | 4 -> Register.step r (* noop, cycle += 1*)
  | _ ->
      Scanf.sscanf s "addx %d" (fun value ->
          Register.step r;
          Register.step r;
          Register.add r value)

let parse_line_v2 r s =
  match String.length s with
  | 4 -> Register.step_v2 r (* noop, cycle += 1*)
  | _ ->
      Scanf.sscanf s "addx %d" (fun value ->
          Register.step_v2 r;
          Register.step_v2 r;
          Register.add r value)

let parse_file f parse_line_v =
  let chan = open_in f in
  let register = Register.create () in
  let rec loop () =
    match In_channel.input_line chan with
    | None -> ()
    | Some l ->
        parse_line_v register l;
        loop ()
  in
  loop ();
  register.total

let run () =
  Printf.printf "[10] - Part 1: %d\n" @@ parse_file filename parse_line;
  Printf.printf "[10] - Part 2: %d\n" @@ parse_file filename parse_line_v2
