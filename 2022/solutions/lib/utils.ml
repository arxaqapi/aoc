(*
 (* Stop after n elements encountered *)
List.fold_left_first_n   
*)

module Array = struct
  include Array

  let iter2i f a b =
    if length a <> length b then
      invalid_arg "Array.iter2: arrays must have the same length"
    else
      for i = 0 to length a - 1 do
        f i (unsafe_get a i) (unsafe_get b i)
      done
end
