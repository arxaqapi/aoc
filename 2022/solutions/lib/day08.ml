let filename = "data/08_input"

(*
A tree is visible if all of the other trees between it and the edge are smaller (same row & col)   
*)

module Tree = struct
  type t = { is_visible : bool option; height : int }

  let is_visible t = t.is_visible
  let create h = { is_visible = None; height = h }
end

module Forest = struct
  type t = {
    forest : (int * int, Tree.t) Hashtbl.t;
    (* visible_tree_count : int; *)
    width : int;
    height : int;
  }

  let get_tree f i j = Hashtbl.find_opt f.forest (i, j)

  let add_tree f i j h =
    match get_tree f i j with
    (* deduplication *)
    | Some _ -> failwith "Already exists"
    | None -> Hashtbl.add f.forest (i, j) (Tree.create h)

  (* only check for a straight line, no recursion *)
  let solve_visibility _f _i _j =
    let _directions = [ (-1, 0); (1, 0); (0, -1); (0, 1) ] in

    (* let rec solver i j = match get_tree f i j with *)
    (* | None -> failwith "No tree found" *)
    (* | Some x -> if (match x.is_visible with None -> false | Some t -> t) then  *)
    ()

  (* let is_visible f i j =
       (* Check if not out of bounds (<0, <0) *)
       if i < 0 || j < 0 || i >= f.height || j >= f.width then true
       else Hashtbl.find f.forest (i, j) |> Tree.is_visible

     let add_tree f i j h =
       match Hashtbl.find_opt f (i, j) with
       (* deduplication *)
       | Some _ -> failwith "Already exists"
       | None -> Hashtbl.add f (i, j) (Tree.create h)

     let get_tree f i j =
       match Hashtbl.find_opt f.forest (i, j) with None -> failwith "inexistant"

     let tree_is_visible f i j =
       let neighbour_offset = [ (-1, 0); (1, 0); (0, -1); (0, 1) ] in
       let rec search_visibility offs =
         match offs with
         | [] -> false
         | (i_off, j_off) :: tl ->
             let current_height = 0 in
             (* wtf *)
             let neighbour = get_tree f (i + i_off) (j + j_off) in
             if
               is_visible f (i + i_off) (j + j_off)
               && neighbour.height < current_height
             then true
             else search_visibility tl
       in
       search_visibility neighbour_offset *)
end

(* return true if is visible, recursive check base on struct (use ) *)

(*
forest.add_tree height: ! None visibility
forest.get_tree i j: -> None or Tree or Out_of_bounds
forest.solve_visibility i j: -> recursive search to see if visible, solve height conditions
    add visibility rules :)

*)
