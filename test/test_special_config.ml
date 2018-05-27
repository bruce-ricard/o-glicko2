(*
 * Glicko2 library. Implements the Glicko2 algorithm.
 * Copyright (C) 2018-present Bruce Ricard
 *
 * This file is part of glicko2.
 *
 * Glicko2 is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
 * (at your option) any later version.
 *
 * Glicko2 is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Glicko2. If not, see <http://www.gnu.org/licenses/>.
 *)




module GlickoLowTau =
  Glicko2.Make(
      struct
        let tau = `Low
        let epsilon = `Default
        let max_volatility_iterations = `Default
        let default_rating = `Default
        let default_deviation = `Default
        let default_volatility = `Default
      end
    )

let test_low_tau () =
  let open GlickoLowTau in
  let open SingleGame in
  let dp () =
    match default_player () with
    | `Ok p -> p
    | _ -> Alcotest.fail "default player shouldn't fail"
  in
  let game_result =
    {
      player1 = {rating = 500.; rating_deviation = 350.; volatility = 0.5;};
      player2 = dp ();
      game_outcome = `Player1Win
    }
  in
  match rate game_result with
      | `Ok({
              new_player1 = {volatility;};
          }) ->
     Alcotest.check
       (Alcotest.float 1e-3)
       "volatility should stay relatively low"
       0.501
       volatility
  | _ -> Alcotest.fail "should return new ratings"

module GlickoHighTau =
  Glicko2.Make(
      struct
        let tau = `High
        let epsilon = `Default
        let max_volatility_iterations = `Default
        let default_rating = `Default
        let default_deviation = `Default
        let default_volatility = `Default
      end
    )

let test_high_tau () =
  let open GlickoHighTau in
  let open SingleGame in
  let dp () =
    match default_player () with
    | `Ok p -> p
    | _ -> Alcotest.fail "default player shouldn't fail"
  in
  let game_result =
    {
      player1 = {
        rating = 500.;
        rating_deviation = 350.;
        volatility = 0.5;
      };
      player2 = dp ();
      game_outcome = `Player1Win
    }
  in
  match rate game_result with
      | `Ok({
              new_player1 = {volatility;};
          }) ->
     Alcotest.check
       (Alcotest.float 1e-3)
       "volatility should go relatively high"
       15.012
       volatility
  | _ -> Alcotest.fail "should return new ratings"

module GlickoLargeEpsilon =
  Glicko2.Make(
      struct
        let tau = `High
        let epsilon = `Manual 3.
        let max_volatility_iterations = `Default
        let default_rating = `Default
        let default_deviation = `Default
        let default_volatility = `Default
      end
    )

let test_large_epsilon () =
  let open GlickoLargeEpsilon in
  let open SingleGame in
  let dp () =
    match default_player () with
    | `Ok p -> p
    | _ -> Alcotest.fail "default player shouldn't fail"
  in
  let game_result =
    {
      player1 = {
        rating = 500.;
        rating_deviation = 350.;
        volatility = 5.;
      };
      player2 = dp ();
      game_outcome = `Player1Win
    }
  in
  match rate game_result with
      | `Ok({
              new_player1 = {volatility;};
          }) ->
     Alcotest.check
       (Alcotest.float 1e-2)
       ("volatility should be quite off of the value it gets to "
        ^ "when epsion is smaller (~26.38)")
       18.85
       volatility
  | _ -> Alcotest.fail "should return new ratings"


module GlickoSmallEpsilon =
  Glicko2.Make(
      struct
        let tau = `High
        let epsilon = `Manual 1e-15
        let max_volatility_iterations = `Default
        let default_rating = `Default
        let default_deviation = `Default
        let default_volatility = `Default
      end
    )

let test_small_epsilon () =
  let open GlickoSmallEpsilon in
  let open SingleGame in
  let dp () =
    match default_player () with
    | `Ok p -> p
    | _ -> Alcotest.fail "default player shouldn't fail"
  in
  let game_result =
    {
      player1 = {
        rating = 500.;
        rating_deviation = 350.;
        volatility = 5.;
      };
      player2 = dp ();
      game_outcome = `Player1Win
    }
  in
  match rate game_result with
      | `Ok({
              new_player1 = {volatility;};
          }) ->
     Alcotest.check
       (Alcotest.float 1e-2)
       ""
       26.38
       volatility
  | _ -> Alcotest.fail "should return new ratings"

module GlickoDefaultPlayer =
  Glicko2.Make(
      struct
        let tau = `Default
        let epsilon = `Default
        let max_volatility_iterations = `Default
        let default_rating = `Manual 101
        let default_deviation = `Manual 2
        let default_volatility = `Manual 3.
      end
    )

let test_default_player () =
  let open Test_utils in
  let open GlickoDefaultPlayer in
  let open SingleGame in
  match GlickoDefaultPlayer.SingleGame.default_player () with
  | `Ok
    {
      rating;
      rating_deviation;
      volatility;
    } ->
     begin
       let check =
         Alcotest.check
           (Alcotest.float 1e-5)
           "default player"
       in
       check 101. rating;
       check 2. rating_deviation;
       check 3. volatility;
     end
  | _ -> Alcotest.fail "should return default player"

let suite = [
    "low tau", `Quick, test_low_tau;
    "high tau", `Quick, test_high_tau;
    "large epsilon", `Quick, test_large_epsilon;
    "small epsilon", `Quick, test_small_epsilon;
    "default player", `Quick, test_default_player;
  ]
