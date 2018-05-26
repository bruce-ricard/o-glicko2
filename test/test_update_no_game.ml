
open Glicko2.Default
open SingleGame
open Test_utils

let update =
  update_player_after_not_player_in_rating_period

let test_update_only_deviation () =
  let p = default_player () in
  let new_p = update p in
  match new_p with
  | `Ok {rating; rating_deviation; volatility} ->
     begin
       Alcotest.check
         (Alcotest.float 1e-2)
         "rating shouldn't change"
         1500.
         rating;
       Alcotest.check
         (Alcotest.float 1e-2)
         "volaility shouldn't change"
         0.06
         volatility
     end
  | `Error _ -> Alcotest.fail "Shouldn't error out"

let test_update_deviation () =
  let p = default_player () in
  let new_p = update p in
  match new_p with
  | `Ok {rating_deviation} ->
     begin
       Alcotest.check
         (Alcotest.float 1e-2)
         "rating deviation should be updated"
         350.155
         rating_deviation;
     end
  | `Error _ -> Alcotest.fail "Shouldn't error out"


let test_update_deviation2 () =
  let p = default_player ~rating_deviation:5 () in
  let new_p = update p in
  match new_p with
  | `Ok {rating_deviation} ->
     begin
       Alcotest.check
         (Alcotest.float 1e-2)
         "rating deviation should be updated"
         11.56
         rating_deviation;
     end
  | `Error _ -> Alcotest.fail "Shouldn't error out"

let test_update_deviation_with_high_volatility () =
  let p = {rating = 1500.; rating_deviation = 5.; volatility = 1.} in
  let new_p = update p in
  match new_p with
  | `Ok {rating_deviation} ->
     begin
       Alcotest.check
         (Alcotest.float 1e-2)
         "rating deviation should be updated"
         173.79
         rating_deviation;
     end
  | `Error _ -> Alcotest.fail "Shouldn't error out"

let update_suite = [
    "update only touches deviation", `Quick, test_update_only_deviation;
    "update updates deviation", `Quick, test_update_deviation;
    "update updates deviation", `Quick, test_update_deviation2;
    "volatility increases deviation", `Quick, test_update_deviation_with_high_volatility;
  ]
