open Core.Std

module Glicko2 = Glicko2.Default.LowLevel

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

let bob_opponent = Glicko2.player_to_opponent bob
let alice_game_results =
  Glicko2.{
    player = alice;
    games = {opponent = bob_opponent; result = `Win},
            [{opponent = bob_opponent; result = `Lose}]
  }

let new_alice =
  match Glicko2.rate alice_game_results with
  | Ok a -> a
  | Error _ -> failwith "deal with error"

let alice_opponent = Glicko2.player_to_opponent alice
let bob_game_results =
  Glicko2.{
    player = bob;
    games = {opponent = alice_opponent; result = `Lose},
            [{opponent = alice_opponent; result = `Win}]
  }

let new_bob =
  match Glicko2.rate bob_game_results with
  | Ok b -> b
  | Error _ -> failwith "deal with error"

let new_charlie =
  match
    Glicko2.update_player_after_not_player_in_rating_period charlie
  with
  | Ok p -> p
  | Error _ -> failwith "deal with error"

(* Those are the players you have at the end *)
let new_player = [new_alice, new_bob, new_charlie]
