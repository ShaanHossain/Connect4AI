#use "sig_game.ml" ;;

module AIPlayer = functor (Game: GAME) ->
struct
  module PlayerGame = Game

  (* TODO *)

  open Game;;

  type move = Move of int

  let rec max_mult : float list -> float = function
  | hd::[] -> hd
  | hd::tl -> (max hd (max_mult tl))
  | _ -> failwith "shouldn't get here";;

  let rec min_mult : float list -> float = function
  | hd::[] -> hd
  | hd::tl -> (min hd (min_mult tl))
  | _ -> failwith "shouldn't get here";;

  let successors : state -> state list = function s ->
  (List.map (next_state s) (legal_moves s));;



  let rec mini_max : state * int -> float = function
  | s, 3 -> estimate_value s
  | s, c ->
    if ((c mod 2) = 0)
    then (max_mult (List.map (mini_max_curried c) (successors s)))
    else (min_mult (List.map (mini_max_curried c) (successors s)))

  and mini_max_curried : int -> state -> float = function n -> function s ->
    mini_max (s, n);;

  let rec find_best_state : state list -> state = function
  | hd::[] -> hd
  | hd::tl ->
    let h = mini_max (hd, 1)
    and t = mini_max ((find_best_state tl), 1)
    in
    if (h > t)
    then hd
    else (find_best_state tl)
  | _ -> failwith "out of bounds for state list";;

  let rec find_best_move : state * state list * int -> move = function
  | s, [], c -> Move c
  | s, hd::tl, c -> if s = hd then (Move c) else find_best_move (s, tl, (c + 1));;

  let make_next_move : state -> move = function s ->
  find_best_move ((find_best_state (successors s)), (successors s), 0);;

  let next_move = make_next_move;;

end ;;

(* TODO: test cases for AIPlayer *)
