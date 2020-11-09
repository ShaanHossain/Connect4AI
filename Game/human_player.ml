#use "sig_game.ml" ;;

module HumanPlayer = functor (Game: GAME) ->
struct
  module PlayerGame = Game

  let rec next_move s =
    try
      let m = PlayerGame.move_of_string (read_line ()) in
      (* TODO: replace the below expression (between the if and then)
       *       with the proper functionality *)
      (* m represents a legal move *)
      if (List.mem m (Game.legal_moves s)) then m
      else
        let () = print_endline "Illegal move."
        in next_move s
    with
      End_of_file -> failwith "exiting."
    | Failure message -> print_endline message ; next_move s
end ;;

(* Because next_move involves read input, you don't need to test it here.
 * If you use any helper procedures in TestHumanPlayer, test them here. *)
