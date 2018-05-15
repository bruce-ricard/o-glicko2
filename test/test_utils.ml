open Glicko2
open SingleGame

let player_to_string {rating; rating_deviation; volatility} =
  Printf.sprintf
    "{rating = %f; rating_deviation = %f; volatlity = %f}"
    rating
    rating_deviation
    volatility

let compare_players (=) p1 p2 =
  p1.rating = p2.rating
  &&
    p1.rating_deviation = p2.rating_deviation
  &&
    p1.volatility = p2.volatility

let player =
  Alcotest.testable
    (Fmt.of_to_string player_to_string)
    (compare_players (fun x y -> abs_float (x -. y) < 1e-2))

let ratings_to_string {new_player1; new_player2} =
  Printf.sprintf
    "{new_player1: %s; new_player2: %s}"
    (player_to_string new_player1)
    (player_to_string new_player2)

let rate_result_to_string = function
  | NewRatings(ratings) ->
     Printf.sprintf
       "NewRatings(%s)"
       (ratings_to_string ratings)
  | InvalidVolatility -> "InvalidVolatlity"
  | InternalError s -> Printf.sprintf "InternalError(%s)" s

let rate_result_t =
  Alcotest.testable (Fmt.of_to_string rate_result_to_string)
                    (=)

let player_return_to_string =
  let open Printf in
  function
  | Player p -> sprintf "Player(%s)" (player_to_string p)
  | Error s -> sprintf "Error(%s)" s

let player_return =
  Alcotest.testable (Fmt.of_to_string player_return_to_string)
                    (=)

let default_player ?rating ?rating_deviation () =
  match default_player ?rating ?rating_deviation () with
  | Player p -> p
  | _ -> Alcotest.fail "default player should be created"
