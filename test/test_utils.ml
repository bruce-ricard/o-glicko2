open Core.Std
open Glicko2.Default
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
    (compare_players (fun x y -> Float.abs (x -. y) < 1e-2))

let ratings_to_string {new_player1; new_player2} =
  Printf.sprintf
    "{new_player1: %s; new_player2: %s}"
    (player_to_string new_player1)
    (player_to_string new_player2)

let rate_result_to_string = function
  | Ok(ratings) ->
     Printf.sprintf
       "NewRatings(%s)"
       (ratings_to_string ratings)
  | Error s -> Printf.sprintf "Error(%s)" s

let rate_result_t =
  Alcotest.testable (Fmt.of_to_string rate_result_to_string)
                    (=)

type err = [ `ExceededIterations
           | `InvalidArgument of string
           | `InvalidVolatility
           | `UnknownError of string ]

let (error_to_string : err -> string) = function
  | `InvalidVolatility -> "`InvalidVolatility"
  | `ExceededIterations -> "`ExceededIterations"
  | `UnknownError s -> Printf.sprintf "`UnkownError(%s)" s
  | `InvalidArgument s -> Printf.sprintf "`InvalidArgument(%s)" s

let player_return_to_string =
  let open Printf in
  function
  | Ok p -> sprintf "Player(%s)" (player_to_string p)
  | Error e -> sprintf "Error(%s)" (error_to_string e)

let player_return =
  Alcotest.testable (Fmt.of_to_string player_return_to_string)
                    (=)

let default_player ?rating ?rating_deviation () =
  match default_player ?rating ?rating_deviation () with
  | Ok p -> p
  | _ -> Alcotest.fail "default player should be created"
