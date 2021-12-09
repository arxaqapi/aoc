
let filename = "09_input"

let chan = open_in filename

let data = 
  Array.init 100 (fun i -> input_line chan 
  |> String.to_seq 
  |> (fun line -> Seq.map (fun e -> int_of_char e - Char.code '0') line)
  |> Array.of_seq)

let is_low data i j = 
  let l = (Array.length data) - 1 in
  let trait i j = match i, j with
    | 0, 0 -> (* top left *)
      data.(i).(j) < data.(i).(j + 1)
      && data.(i).(j) < data.(i + 1).(j)
    | ii, 0 when ii=l -> (* bottom left *)
      data.(i).(j) < data.(i).(j + 1)
      && data.(i).(j) < data.(i - 1).(j)
    | 0, jj when jj=l -> (* top right *)
      data.(i).(j) < data.(i).(j - 1)
      && data.(i).(j) < data.(i + 1).(j)
    | ii, jj when jj=l && ii=l -> (* bottom right *)
      data.(i).(j) < data.(i).(j - 1)
      && data.(i).(j) < data.(i - 1).(j)
    | _, 0 -> (* left col *)
      data.(i).(j) < data.(i).(j + 1)
      && data.(i).(j) < data.(i - 1).(j) 
      && data.(i).(j) < data.(i + 1).(j)
    | _, jj when jj=l -> (* right col *)
      data.(i).(j) < data.(i).(j - 1)
      && data.(i).(j) < data.(i - 1).(j) 
      && data.(i).(j) < data.(i + 1).(j)
    | 0, _ -> (* top row *)
      data.(i).(j) < data.(i).(j + 1)
      && data.(i).(j) < data.(i).(j - 1)
      && data.(i).(j) < data.(i + 1).(j)
    | ii, _ when ii=l -> (* bottom row *)
      data.(i).(j) < data.(i).(j + 1)
      && data.(i).(j) < data.(i).(j - 1)
      && data.(i).(j) < data.(i - 1).(j)
    | _, _ -> (* rest *)
      data.(i).(j) < data.(i + 1).(j) 
      && data.(i).(j) < data.(i - 1).(j) 
      && data.(i).(j) < data.(i).(j - 1) 
      && data.(i).(j) < data.(i).(j + 1) 
  in trait i j

let solve arr = 
  let tot = ref 0 in
  let len = Array.length arr - 1 in
  for i = 0 to len do
    for j = 0 to len do
      if is_low arr i j 
        then tot := !tot + arr.(i).(j) + 1
    done
  done;
  !tot

let () =
  Printf.printf "day9.1: %i" @@ solve data;
  assert (data.(0).(0) = 4);
  assert (data.(99).(4) = 9);