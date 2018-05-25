open Core.Std

module Glicko2 = Glicko2.Default.SingleGame

let alice =
  match Glicko2.default_player () with
  | Ok p -> p
  | Error _ -> failwith "error creating player"

let bob =
  match Glicko2.default_player ~rating:1800 () with
  | Ok p -> p
  | Error _ -> failwith "error creating player"

let charlie =
  match Glicko2.default_player () with
  | Ok p -> p
  | Error _ -> failwith "error creating player"

let first_game_result =
  Glicko2.{
          player1 = alice;
          player2 = bob;
          game_outcome = `Player1Win;
  }

let result =
  match Glicko2.rate first_game_result with
  | Ok r -> r
  | Error _ -> failwith "deal with error"

let new_alice, new_bob =
  Glicko2.(result.new_player1, result.new_player2)

let second_game_result =
  Glicko2.{
          player1 = new_alice;
          player2 = new_bob;
          game_outcome = `Player2Win;
  }

let result =
  match Glicko2.rate second_game_result with
  | Ok r -> r
  | Error _ -> failwith "deal with error"

let new_alice, new_bob =
  Glicko2.(result.new_player1, result.new_player2)

let new_charlie =
  match
    Glicko2.update_player_after_not_player_in_rating_period charlie
  with
  | Ok p -> p
  | Error _ -> failwith "deal with error"

(* Those are the players you have at the end *)
let new_player = [new_alice, new_bob, new_charlie]
