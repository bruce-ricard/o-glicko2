open Core.Std
open Test_utils
module Glicko2 = Glicko2.Default
open Glicko2
open SingleGame

let default_player =
  (default_player :> ?rating:int ->
                     ?rating_deviation:int ->
                     unit ->
                     (Glicko2.player, Test_utils.err) Result.t
  )

let test_default_player () =
  Alcotest.check
    player_return
    "default player should be correct"
    (Ok
       {
         rating = 1500.;
         rating_deviation = 350.;
         volatility = 0.06;
       }
    )
    (default_player ())

let test_default_player_sets_rating () =
  Alcotest.check
    player_return
    "default player should be correct"
    (Ok
       {
         rating = 1000.;
         rating_deviation = 350.;
         volatility = 0.06;
       }
    )
    (default_player ~rating:1000 ())

let test_default_player_sets_deviation () =
  Alcotest.check
    player_return
    "default player should be correct"
    (Ok
       {
         rating = 1500.;
         rating_deviation = 150.;
         volatility = 0.06;
       }
    )
    (default_player ~rating_deviation:150 ())

let test_default_player_low_rating () =
  Alcotest.check
    player_return
    "default player should be correct"
    (Error (
         `InvalidArgument
          "rating cannot be lower than 100"
    ))
    (default_player ~rating:99 ())

let test_default_player_low_deviation () =
  Alcotest.check
    player_return
    "default player should be correct"
    (Error
       (`InvalidArgument
       "rating_deviation cannot be negative")
    )
    (default_player ~rating_deviation:(-1) ())

let test_default_player_high_deviation () =
  Alcotest.check
    player_return
    "default player should be correct"
    (Error
       (`InvalidArgument
       "rating_deviation cannot be greater than 350")
    )
    (default_player ~rating_deviation:(351) ())

let default_player_suite = [
    "creation", `Quick, test_default_player;
    "correct rating", `Quick, test_default_player_sets_rating;
    "correct deviation", `Quick, test_default_player_sets_deviation;
    "low rating", `Quick, test_default_player_low_rating;
    "low rating deviation", `Quick, test_default_player_low_deviation;
    "high rating deviation", `Quick, test_default_player_high_deviation;
  ]
