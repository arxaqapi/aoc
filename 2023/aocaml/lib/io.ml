let load_all_data f = In_channel.input_all (open_in f)

let load_as_list f =
  let chan = open_in f in
  let rec loop acc =
    match In_channel.input_line chan with
    | None -> List.rev acc
    | Some line -> line :: acc |> loop
  in
  loop []
