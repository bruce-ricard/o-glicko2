

module Glicko2 = Glicko2.Default
open Glicko2.LowLevel
open Test_utils

let player_to_opponent
      {
        Glicko2.rating;
        rating_deviation;
      } =
  {
    Glicko2.LowLevel.rating = rating;
    rating_deviation = rating_deviation;
  }

let test_rate_one_game () =
  let game_results =
    {
      player = default_player ();
      games = {
          opponent = player_to_opponent (default_player ());
          result = `Win
        },[];
    } in
  let expected_new_player =
    Glicko2.{
      rating = 1662.31;
      rating_deviation = 290.32;
      volatility = 0.06;
    }
  in
  let open Glicko2 in
  match rate game_results with
  | `Ok new_player ->
     Alcotest.check
       player
       "Player should have higher rating"
       expected_new_player
       new_player
  | _ ->  Alcotest.fail "shouldn't internal error"

let test_rate_two_games () =
  let game1 =
    {
      opponent = player_to_opponent (default_player ());
      result = `Win
    } in
  let game_results =
    {
      player = default_player ();
      games = game1,[game1];
    } in
  let expected_new_player =
    Glicko2.{
      rating = 1747.31;
      rating_deviation = 253.40;
      volatility = 0.06;
    }
  in
  let open Glicko2 in
  match rate game_results with
  | `Ok new_player ->
     Alcotest.check
       player
       "Player should have higher rating"
       expected_new_player
       new_player
  | _ ->  Alcotest.fail "shouldn't internal error"


let rate_suite =  [
    "one game played", `Quick, test_rate_one_game;
    "two games played", `Quick, test_rate_two_games;
  ]
