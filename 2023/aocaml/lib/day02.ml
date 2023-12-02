open Angstrom

module Game_set = struct
  type t = { red : int; green : int; blue : int }

  let init ?(r = 0) ?(g = 0) ?(b = 0) () = { red = r; green = g; blue = b }

  let set col value gs =
    match col with
    | "red" -> { gs with red = value }
    | "green" -> { gs with green = value }
    | "blue" -> { gs with blue = value }
    | _ -> failwith "color not supported"

  let is_correct game_set base =
    game_set.red <= base.red
    && game_set.green <= base.green
    && game_set.blue <= base.blue

  let pp gs = Printf.printf "r: %d, g %d, b: %d ; " gs.red gs.green gs.blue

  let get col gs =
    match col with
    | "red" -> gs.red
    | "green" -> gs.green
    | "blue" -> gs.blue
    | _ -> failwith "color not supported"

  let iprod gs = gs.red * gs.green * gs.blue
end

let comparison_game = Game_set.init ~r:12 ~g:13 ~b:14 ()

module Game = struct
  type t = { game_id : int; sets : Game_set.t list }

  let init game_id game_sets = { game_id; sets = game_sets }

  let games_are_correct game =
    List.fold_left
      (fun b e -> b && Game_set.is_correct e comparison_game)
      true game.sets

  let get_fewest game =
    List.fold_left
      (fun acc gs ->
        Game_set.set "red"
          (max (Game_set.get "red" gs) (Game_set.get "red" acc))
          acc
        |> Game_set.set "green"
             (max (Game_set.get "green" gs) (Game_set.get "green" acc))
        |> Game_set.set "blue"
             (max (Game_set.get "blue" gs) (Game_set.get "blue" acc)))
      (Game_set.init ()) game.sets

  let pp game =
    Printf.printf "Game %d: " game.game_id;
    List.iter Game_set.pp game.sets
end

let p_space = char ' '

let p_integer =
  take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string

let p_game_id = string "Game " *> p_integer <* string ": "
let p_color = string "red" <|> string "green" <|> string "blue"

let p_pair =
  p_integer >>= fun value ->
  p_space *> p_color >>= fun color -> (value, color) |> return

let p_hand = sep_by (string ", ") p_pair
let p_all_hands = sep_by1 (string "; ") p_hand

let line =
  p_game_id >>= fun id ->
  p_all_hands >>= fun all_hands ->
  Game.init id
    (List.map
       (List.fold_left
          (fun b (value, color) -> Game_set.set color value b)
          (Game_set.init ()))
       all_hands)
  |> return

let p_all_lines = sep_by (char '\n') line
let get_data () = In_channel.input_all (open_in "data/02_input.txt")

let solve_part_1 input =
  parse_string ~consume:Prefix p_all_lines input |> function
  | Ok games ->
      List.fold_left
        (fun acc g ->
          Game.games_are_correct g |> function
          | true -> acc + g.game_id
          | false -> acc)
        0 games
  | Error e -> failwith e

let solve_part_2 input =
  parse_string ~consume:Prefix p_all_lines input |> function
  | Ok games ->
      List.fold_left
        (fun acc game -> acc + (Game.get_fewest game |> Game_set.iprod))
        0 games
  | Error e -> failwith e

let solve () =
  let input = get_data () in
  Printf.printf "[02] - Part 1: %d\n" @@ solve_part_1 input;
  Printf.printf "[02] - Part 2: %d\n" @@ solve_part_2 input
