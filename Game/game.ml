#use "CS17setup.ml" ;;
#use "sig_game.ml" ;;

module Connect4 =
struct
  type which_player = P1 | P2

  type status =
  | Win of which_player
  | Draw
  | Ongoing of which_player

  type board = int list list

  type state = status * board

  (* NOTE Why is there a problem here *)

  type move = Move of int

  let rows = 6;;

  let columns = 7;;

  let rec constructs_board : int * int * int list list -> board = function
  | 0, 1, b -> b
  | 0, c, hd::tl-> constructs_board (0, (c - 1), hd::hd::tl)
  | r, c, hd::tl -> constructs_board ((r - 1), c, (0::hd)::tl)
  | _ -> failwith "out of bounds exception";;

  (* PRINTING FUNCTIONS *)
  (* let string_of_player : which_player -> string =
  function p -> if (p = P1) then "P1" else "P2";;

  let string_of_state  : state        -> string =
  function s -> "0";;

  let string_of_move   : move         -> string =
  function (Move n) -> (string_of_int n);; *)

  let string_of_player : which_player -> string = function p ->
    match p with
    | P1 -> "P1"
    | P2 -> "P2";;

  let string_of_status : status -> string = function s ->
    match s with
    | Ongoing P1 -> "\n It is P1's turn."
    | Ongoing P2 -> "\n It is P2's turn."
    | Win P1 -> "\n P1 has won the game."
    | Win P2 -> "\n P2 has won the game."
    | Draw -> "The game is a draw."

  let rec string_of_row : int list -> string  = function row ->
    match row with
      | [] -> "\n"
      | hd::tl -> (string_of_int hd) ^ "   " ^ (string_of_row tl);;

  let rec string_of_board : board -> string = function board ->
    match board with
      | [] -> " "
      | hd :: tl -> (string_of_row hd) ^ (string_of_board tl) ;;

  let rec string_of_state : status * board -> string = function s, b ->
   (string_of_board b) ^ (string_of_status s);;

  let string_of_move : move -> string = function (Move n) ->
    (string_of_int n);;

  (* GAME LOGIC *)

  (* the state of the game when it begins *)
  let initial_state : state = ((Ongoing P1), constructs_board (rows, columns, [[]]));;

  (* NOTE Hey Carleen should we have it as an empty array or should we
  represent the absence as something like None and make our pieces options *)

  (* produces the set of legal moves at a state, represented as a list *)

  let rec paralegal : (('a list list) * int) -> move list =
  function (s, counter) ->
  match (s, counter) with
  | [], counter -> []
  | (hd::tl, counter) -> if (List.hd hd) = 0
              then (Move counter)::(paralegal (tl, (counter + 1)))
              else paralegal (tl, (counter + 1));;

  let rec legal_moves : state -> move list = function (s, b) ->
  if ((s = (Win P1)) || (s = (Win P2)) || (s = (Draw)))
  then []
  else paralegal (b, 1);;

  (* returns the status of the game at the given state *)
  let game_status : state -> status = function (s, b) -> s;;

  (* given a state and a legal move, yields the next state
     Note that this procedure is curried. *)

  let rec drop_piece : int * int list -> int list = function
   | p, [] -> []
   | p, 0::[] -> p::[]
   | p, 0::hd::tl -> if (hd=0) then 0::drop_piece (p, (hd::tl))
                               else p::hd::tl
   | p,_ -> failwith "legal move columns must start with 0";;

  let rec pick_place : (int * move * int list list) -> int list list =
    function (p, m, lst) -> match (p, m, lst) with
    | (p, (Move 1), hd::tl) -> drop_piece (p, hd)::tl
    | (p, (Move m), hd::tl) -> hd::(pick_place (p, (Move (m - 1)), tl))
    | _ -> failwith "out of bounds exception";;

  (* NOTE what case would our pick_place fail *)

  (* let next_state : state -> move -> state =
   function (s, b) -> function m ->
   if ((s = (Win P1)) || (s = (Win P2)) || (s = (Draw)))
   then (s, b) (*NOTE maybe a failwith*)
   else (if (s=Ongoing P1) then (Ongoing P2, (pick_place (1, m, b))) (* Watch the 2 *)
                           else (Ongoing P1, (pick_place (2, m, b))));; *)

  (* SPECIFIC TO HUMAN PLAYERS *)

  (* for transforming player input into
   * the internal representation of a move *)
  let move_of_string : string -> move = function s -> Move (int_of_string s);;

  (* SPECIFIC TO AI PLAYERS *)

  let rec k_subsets : int list * int -> int list list = function
    | _, 0 -> [[]]
    | [], k -> [[]]
    | hd::tl, k -> ((k_subsets (tl, k)) @ (List.map (function x -> hd::x) (k_subsets (tl, (k - 1)))));;

  let rec four_subsets : int list -> int list list =
  function (lst) -> (k_subsets (lst, 4));;

  (* let rec subsets : int list -> int list list = function
  | [] -> [[]]
  | hd::tl -> (List.map (function x -> hd::x) (subsets tl)) @ (subsets tl);; *)

  let rec orderP_help : int list * int list -> bool = function
  | _, [] -> true
  | [], _ -> false
  | hd1::tl1, hd2::tl2 -> (hd1 = hd2) && orderP_help (tl1, tl2);;

  let rec orderP : int list * int list -> bool = function
  | _, [] -> true
  | [], _ -> false
  | hd1::tl1, hd2::tl2 -> if (hd1 = hd2)
                          then orderP_help (tl1, tl2)
                          else orderP (tl1, tl2);;

  let rec orderP_map : int list * int list list -> int list list = function
  | c, [] -> []
  | c, hd::tl -> if (orderP (c, hd)) then hd::(orderP_map (c, tl)) else (orderP_map (c, tl));;

  let rec ordered_sub : int list -> int list list = function c ->
    (orderP_map (c, (four_subsets c)));;

  let rec same_list : int list -> bool = function
    | [] -> true
    | 0::_ -> false
    | hd::[] -> true
    | hd::tl -> (hd = (List.hd tl)) && (same_list tl);;

  let rec comp_val : int list -> float = function lst ->

    if (lst = [])
    then 0.0
    else if ((same_list lst) && ((List.hd lst) = 1))
    then
      match (List.length lst) with
        | 0 -> 0.0
        | 1 -> 1.0
        | 2 -> 100.0
        | 3 -> 10000.0
        | 4 -> 1000000.0
        | _ -> failwith "what are you smoking, where can I get some?"
    else if ((same_list lst) && ((List.hd lst) = 2))
    then
      match (List.length lst) with
        | 0 -> 0.0
        | 1 -> -1.0
        | 2 -> -100.0
        | 3 -> -10000.0
        | 4 -> -1000000.0
        | _ -> failwith "what are you smoking, where can I get some?"
    else 0.0;;

  let rec flip : board -> board = function b ->
    match b with
    | [] -> []
    | (hd::[])::tl -> [(List.map List.hd b)]
    | b -> (List.map List.hd b)::(flip (List.map List.tl b));;

  let rec transpose list = match list with
  | [] | [] :: _  -> failwith "A matrix cannot be 0-dimensional."
  | (hd1 :: []) :: t1 -> [List.flatten ((hd1 :: []) :: t1)]
  | (hd1 :: tl1) :: t1 ->
    (hd1 :: List.map List.hd t1) :: transpose (tl1 :: List.map List.tl t1) ;;

  let compute_column_substrings_values : board -> float list = function b ->
    (List.map comp_val (List.flatten (List.map ordered_sub b)));;

  let compute_row_substrings_values : board -> float list = function b ->
    compute_column_substrings_values (flip b);;

  let rec diagonal : int list list * int -> int list list = function
    | [], _ -> []
    | hd::tl, counter -> (if (counter = (List.length hd)) then []
                        else [(List.nth hd counter)]::(diagonal (tl, (counter + 1))));;

  let rec flat_dag : int list list -> int list = function d ->
    List.flatten (diagonal (d, 0));;

  let rec compute_lower_diagonals : int list list -> int list list = function
    | [] -> []
    | hd::tl -> (flat_dag (hd::tl)) :: (compute_lower_diagonals tl);;

  (* let compute_lower_diagonals : board -> int list = function *)

  let compute_left_diagonal_values : board -> float list = function b ->
    List.map comp_val ((compute_lower_diagonals b) @ (compute_lower_diagonals (transpose b)));;

  let compute_right_diagonal_values : board -> float list = function b ->
     compute_left_diagonal_values (flip b);;

  let rec sfl : float list -> float = function
    | [] -> 0.0
    | hd::tl -> hd +. (sfl tl);;

  (* estimate the value (for player 1) of a given state *)
  let estimate_value : state -> float = function (s, b) ->
    ((sfl (compute_column_substrings_values b)) +.
    (sfl (compute_row_substrings_values b)) +.
    (sfl (compute_left_diagonal_values b)) +.
    (sfl (compute_right_diagonal_values b)));;

    (* let next_state : state -> move -> state =
     function (s, b) -> function m ->
     if ((s = (Win P1)) || (s = (Win P2)) || (s = (Draw)))
     then (s, b) (*NOTE maybe a failwith*)
     else (if (s=Ongoing P1) then (Ongoing P2, (pick_place (1, m, b))) (* Watch the 2 *)
                             else (Ongoing P1, (pick_place (2, m, b))));; *)

   let next_state : state -> move -> state =
    function (s, b) -> function m ->
    if (estimate_value (s, b)) > 900000.0 then ((Win P1), b)
    else if (estimate_value (s, b)) < -900000.0 then ((Win P2), b)
    else if ((legal_moves (s, b)) = []) then (Draw, b)
    else (if (s=Ongoing P1) then (Ongoing P2, (pick_place (1, m, b))) (* Watch the 2 *)
                            else (Ongoing P1, (pick_place (2, m, b))));;

end ;;

module MyGame : GAME = Connect4 ;;

(* TODO: implement your game with the rest of the GAME signature *)

(* TODO: test cases for this Game *)

(* Check Expects *)

open Connect4;;

check_expect (string_of_player (P1)) ("P1");;
check_expect (string_of_player (P2)) ("P2");;

check_expect (string_of_board (constructs_board (0,1,[[]]))) "\n ";;
check_expect (string_of_board (constructs_board (1,1,[[]]))) "0   \n ";;
check_expect (string_of_board (constructs_board (1,2,[[]]))) "0   \n0   \n ";;
check_expect (string_of_board (constructs_board (2,1,[[]]))) "0   0   \n ";;
check_expect (string_of_board (constructs_board (2,2,[[]])))
"0   0   \n0   0   \n ";;
check_expect (string_of_board (constructs_board (2,3,[[]])))
"0   0   \n0   0   \n0   0   \n ";;
check_expect (string_of_board (constructs_board (3,3,[[]])))
"0   0   0   \n0   0   0   \n0   0   0   \n ";;
check_expect (string_of_board (constructs_board (6,7,[[]])))
("0   0   0   0   0   0   \n0   0   0   0   0   0   \n0   0   0   0   0   0  "^
" \n0   0   0   0   0   0   \n0   0   0   0   0   0   \n0   0   0   0   0   0"^
"   \n0   0   0   0   0   0   \n ");;

check_expect (string_of_state ((Ongoing P1), [[]]))
("\n " ^ "\n It is P1's turn.") ;;
check_expect (string_of_state ((Ongoing P2), [[]]))
("\n " ^ "\n It is P2's turn.");;
check_expect (string_of_state ((Win P1), [[]]))
"\n \n P1 has won the game.";;
check_expect (string_of_state ((Win P2), [[]]))
"\n \n P2 has won the game.";;
check_expect (string_of_state ((Draw), [[]]))
"\n The game is a draw.";;
check_expect (string_of_state ((Ongoing P1), [[0]]))
"0   \n \n It is P1's turn." ;;
check_expect (string_of_state ((Ongoing P1), [[0]; [0]]))
"0   \n0   \n \n It is P1's turn.";;
check_expect (string_of_state ((Ongoing P1), [[1]; [0]]))
"1   \n0   \n \n It is P1's turn." ;;
check_expect (string_of_state ((Ongoing P1), [[0]; [1]]))
"0   \n1   \n \n It is P1's turn.";;
check_expect (string_of_state ((Ongoing P1), [[1; 1]; [0; 0]]))
"1   1   \n0   0   \n \n It is P1's turn.";;
check_expect (string_of_state ((Ongoing P1), [[0; 1]; [0; 1]]))
"0   1   \n0   1   \n \n It is P1's turn.";;
check_expect (string_of_state ((Ongoing P1), [[1; 1]; [1; 1]]))
"1   1   \n1   1   \n \n It is P1's turn.";;
check_expect (string_of_state ((Ongoing P1), [[1; 2; 3]; [4; 5; 6]]))
"1   2   3   \n4   5   6   \n \n It is P1's turn.";;

check_expect (string_of_move (Move 1)) "1";;
check_expect (string_of_move (Move 2)) "2";;
check_expect (string_of_move (Move 3)) "3";;
check_expect (move_of_string "1")  (Move 1);;
check_expect (move_of_string "2")  (Move 2);;
check_expect (move_of_string "3")  (Move 3);;

check_expect (initial_state) ((Ongoing P1), constructs_board (6,7, [[]]));;
check_expect (legal_moves ((Win P1), [[0; 1]; [0; 1]])) [];;
check_expect (legal_moves ((Draw), [[0; 1]; [0; 2]])) [];;
check_expect (legal_moves ((Ongoing P1), [[1; 2]])) [];;
check_expect (legal_moves ((Ongoing P1), [[0; 1]])) [Move 1];;
check_expect (legal_moves ((Ongoing P1), [[0; 2]; [0; 1]])) [Move 1; Move 2];;
check_expect (legal_moves ((Ongoing P1), [[1; 2]; [2; 1]])) [];;
check_expect (legal_moves ((Ongoing P1), [[2]; [1]; [0]])) [Move 3];;
check_expect (legal_moves ((Ongoing P1), [[1; 2; 2]; [0; 1; 1]; [0; 2; 2];
                                          [1; 1; 1]])) [Move 2; Move 3];;
check_expect (legal_moves ((Ongoing P1), [[1; 2; 2]; [1; 1; 1]; [1; 2; 2];
                                          [1; 1; 1]])) [];;
check_expect (legal_moves ((Ongoing P1), [[0; 1; 1; 2; 2; 1];
                                          [0; 2; 1; 2; 2; 2];
                                          [0; 2; 2; 2; 1; 1];
                                          [2; 1; 1; 1; 2; 1];
                                          [0; 1; 1; 2; 2; 1];
                                          [0; 2; 1; 2; 2; 2];
                                          [0; 2; 2; 2; 1; 1]]))
[Move 1; Move 2; Move 3; Move 5; Move 6; Move 7];;

check_expect (game_status ((Draw), [[]])) (Draw);;
check_expect (game_status ((Ongoing P1), [[]])) (Ongoing P1);;
check_expect (game_status ((Ongoing P2), [[]])) (Ongoing P2);;
check_expect (game_status ((Win P1), [[]])) (Win P1);;
check_expect (game_status ((Win P2), [[]])) (Win P2);;

(*next_state*)
check_expect (string_of_state ((next_state (Ongoing P2,
([[0;  0]; [0;  0]; [0; 0]])) (Move 1))))
("0   2   \n0   0   \n0   0   \n \n It is P1's turn.");;
check_expect (string_of_state ((next_state (Ongoing P2,
([[0;  1]; [0;  0]])) (Move 1))))
 ("2   1   \n0   0   \n \n It is P1's turn.");;
check_expect (string_of_state ((next_state (Ongoing P2,
([[0;  1]; [0;  2]; [0; 1]])) (Move 1))))
("2   1   \n0   2   \n0   1   \n \n It is P1's turn.");;
check_expect (string_of_state ((next_state (initial_state) (Move 1))))
 ("0   0   0   0   0   1   "^
"\n0   0   0   0   0   0   "^
"\n0   0   0   0   0   0   "^
"\n0   0   0   0   0   0   "^
"\n0   0   0   0   0   0   "^
"\n0   0   0   0   0   0   "^
"\n0   0   0   0   0   0   "^
"\n \n It is P2's turn.");;
check_expect (string_of_state ((next_state (Ongoing P2,
([[0;  2;  1;  1;  2;  1];
  [1;  1;  1;  2;  1;  2];
  [1;  2;  1;  2;  2;  1];
  [2;  1;  2;  1;  1;  2];
  [2;  2;  1;  2;  2;  1];
  [1;  1;  1;  2;  1;  2];
  [2;  2;  1;  1;  2;  1]])) (Move 1))))
 ("2   2   1   1   2   1   "^
"\n1   1   1   2   1   2   "^
"\n1   2   1   2   2   1   "^
"\n2   1   2   1   1   2   "^
"\n2   2   1   2   2   1   "^
"\n1   1   1   2   1   2   "^
"\n2   2   1   1   2   1   "^
"\n \n It is P1's turn.");;
check_expect (string_of_state ((next_state (Win P2,
([[0;  1;  1;  1;  2;  1];
  [1;  1;  1;  2;  1;  2];
  [1;  2;  1;  2;  2;  1];
  [2;  1;  2;  1;  1;  2];
  [2;  2;  1;  2;  2;  1];
  [1;  1;  1;  2;  1;  2];
  [2;  2;  1;  1;  2;  1]])) (Move 1))))
 ("0   1   1   1   2   1   "^
"\n1   1   1   2   1   2   "^
"\n1   2   1   2   2   1   "^
"\n2   1   2   1   1   2   "^
"\n2   2   1   2   2   1   "^
"\n1   1   1   2   1   2   "^
"\n2   2   1   1   2   1   "^
"\n \n P1 has won the game.");;
(* Drop_piece is being checked even though it's a helper function because
   I want to demonstrate its functionality in some tricky situations. If
   I tried all drop_piece-dependent edge cases on next_state this program
   would be thousands of lines long, and nobody wants that. *)
check_expect (drop_piece (1, [0; 0; 0; 0; 0; 0; 0]))
                             [0; 0; 0; 0; 0; 0; 1];;
check_expect (drop_piece (1, [0; 0; 0; 0; 0; 0; 2]))
                             [0; 0; 0; 0; 0; 1; 2];;
check_expect (drop_piece (2, [0; 0; 1; 2; 1; 2; 1]))
                             [0; 2; 1; 2; 1; 2; 1];;
check_expect (drop_piece (2, [0; 2; 1; 2; 1; 2; 1]))
                             [2; 2; 1; 2; 1; 2; 1];;

(* test cases for estimate_value *)

check_expect (estimate_value
  (Ongoing P1,
  ([[0;  0;  0;  0;  0;  0];
    [0;  0;  0;  0;  0;  0]]))) 0.0;;
check_expect (estimate_value
  (Ongoing P1,
  ([[0;  0;  0;  0;  0;  1];
    [0;  0;  0;  0;  0;  0]]))) 4.0;;
check_expect (estimate_value
  (Ongoing P2,
  ([[0;  0;  0;  0;  0;  1];
    [0;  0;  0;  0;  0;  2]]))) 2.0;;
check_expect (estimate_value
  (Ongoing P2,
  ([[0;  0;  0;  0;  1;  1];
    [0;  0;  0;  0;  0;  0]]))) 106.0;;
check_expect (estimate_value
  (Ongoing P2,
  ([[0;  0;  0;  0;  1;  1];
    [0;  0;  0;  0;  0;  2]]))) 104.0;;
check_expect (estimate_value
  (Ongoing P2,
  ([[0;  0;  0;  0;  1;  1];
    [0;  0;  0;  0;  2;  2]]))) 2.0;;
check_expect (estimate_value
  (Ongoing P2,
  ([[0;  0;  0;  1;  1;  1];
    [0;  0;  0;  0;  0;  0]]))) 10308.0;;
check_expect (estimate_value
  (Ongoing P2,
  ([[0;  0;  2;  1;  1;  1];
    [0;  0;  0;  0;  0;  0]]))) 10306.0;;
check_expect (estimate_value
  (Ongoing P2,
  ([[0;  0;  1;  1;  1;  2];
    [0;  0;  0;  0;  0;  0]]))) 10302.0;;
check_expect (estimate_value
  (Ongoing P2,
  ([[0;  0;  1;  1;  1;  1];
    [0;  0;  0;  0;  0;  0]]))) 1040610.0;;
check_expect (estimate_value
  (Ongoing P2,
  ([[0;  0;  0;  1;  1;  1];
    [0;  0;  0;  2;  2;  2]]))) 2.0;;
check_expect (estimate_value
  (Ongoing P2,
  ([[0;  0;  1;  1;  1;  1];
    [0;  0;  0;  2;  2;  2]]))) 1030304.0;;
check_expect (estimate_value
  (Ongoing P2,
  ([[0;  0;  0;  1;  1;  1];
    [0;  0;  2;  2;  2;  2]]))) (-1030300.);;
