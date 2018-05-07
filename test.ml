open Glicko2

let player_to_string {rating; rating_deviation; volatility} =
  Printf.sprintf
    "{rating = %f; rating_deviation = %f; volatlity = %f}"
    rating
    rating_deviation
    volatility

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
  | InternalError -> "InternalError"

let rate_result_t =
  Alcotest.testable (Fmt.of_to_string rate_result_to_string) (=)

let player_return_to_string =
  let open Printf in
  function
  | Player p -> sprintf "Player(%s)" (player_to_string p)
  | Error s -> sprintf "Error(%s)" s

let player_return =
  Alcotest.testable (Fmt.of_to_string player_return_to_string) (=)

let test_default_player () =
  Alcotest.check
    player_return
    "default player should be correct"
    (Glicko2.Player
       {
         rating = 1500.;
         rating_deviation = 350.;
         volatility = 0.06;
       }
    )
    (Glicko2.default_player ())

let test_default_player_sets_rating () =
  Alcotest.check
    player_return
    "default player should be correct"
    (Glicko2.Player
       {
         rating = 1000.;
         rating_deviation = 350.;
         volatility = 0.06;
       }
    )
    (Glicko2.default_player ~rating:1000 ())

let test_default_player_sets_deviation () =
  Alcotest.check
    player_return
    "default player should be correct"
    (Glicko2.Player
       {
         rating = 1500.;
         rating_deviation = 150.2;
         volatility = 0.06;
       }
    )
    (Glicko2.default_player ~rating_deviation:150.2 ())

let test_default_player_low_rating () =
  Alcotest.check
    player_return
    "default player should be correct"
    (Glicko2.Error
       "rating cannot be lower than 100"
    )
    (Glicko2.default_player ~rating:99 ())

let test_default_player_low_deviation () =
  Alcotest.check
    player_return
    "default player should be correct"
    (Glicko2.Error
       "rating_deviation cannot be negative"
    )
    (Glicko2.default_player ~rating_deviation:(-1e-5) ())

let test_default_player_high_deviation () =
  Alcotest.check
    player_return
    "default player should be correct"
    (Glicko2.Error
       "rating_deviation cannot be greater than 350"
    )
    (Glicko2.default_player ~rating_deviation:(350.01) ())

let default_player_suite = [
    "creation", `Quick, test_default_player;
    "correct rating", `Quick, test_default_player_sets_rating;
    "correct deviation", `Quick, test_default_player_sets_deviation;
    "low rating", `Quick, test_default_player_low_rating;
    "low rating deviation", `Quick, test_default_player_low_deviation;
    "high rating deviation", `Quick, test_default_player_high_deviation;
  ]

let default_player () =
  match Glicko2.default_player () with
  | Player p -> p
  | _ -> Alcotest.fail "default player should be created"

let test_simple_rate () =
  let game_result =
    {
      player1 = default_player ();
      player2 = default_player ();
      game_outcome = Draw;
    } in
  match rate_single_game game_result with
  | NewRatings _ -> ()
  | _ -> Alcotest.fail "should return new ratings"

let test_simple_rate2 () =
  let game_result =
    {
      player1 = default_player ();
      player2 = default_player ();
      game_outcome = Draw;
    } in
  match rate_single_game game_result with
  | NewRatings({
        new_player1 = {rating = p1rating;};
        new_player2 = {rating = p2rating;};
    }) ->
     Alcotest.check
       (Alcotest.float 1e-10)
       "ratings shouldn't move from 1500"
       1500.
       p1rating;
     Alcotest.check
       (Alcotest.float 1e-10)
       "ratings shouldn't move from 1500"
       1500.
       p2rating
  | _ -> Alcotest.fail "should return new ratings"

let test_win_changes_rating () =
  let game_result =
    {
      player1 = default_player ();
      player2 = default_player ();
      game_outcome = Player1Win;
    } in
  match rate_single_game game_result with
  | NewRatings({
        new_player1 = {rating = p1rating;};
        new_player2 = {rating = p2rating;};
    }) ->
     Alcotest.check
       (Alcotest.float 1e-3)
       "ratings should be larger"
       1662.31
       p1rating;
     Alcotest.check
       (Alcotest.float 1e-3)
       "rating should be lower"
       1337.69
       p2rating
  | _ -> Alcotest.fail "should return new ratings"

let test_deviation_matters () =
  let game_result =
    {
      player1 = {(default_player ()) with rating_deviation = 100.};
      player2 = {(default_player ()) with rating_deviation = 10.};
      game_outcome = Player1Win;
    } in
  match rate_single_game game_result with
  | NewRatings({
        new_player1 = {rating = p1rating;};
        new_player2 = {rating = p2rating;};
    }) ->
     Alcotest.check
       (Alcotest.float 1e-2)
       "ratings should be larger"
       1526.84
       p1rating;

     Alcotest.check
       (Alcotest.float 1e-2)
       "rating should be lower"
       1499.43
       p2rating;

  | _ -> Alcotest.fail "should return new ratings"

let test_volatility_matters () =
  let game_result =
    {
      player1 = {(default_player ()) with volatility = 1.};
      player2 = {(default_player ()) with volatility = 10.};
      game_outcome = Player1Win;
    } in
  match rate_single_game game_result with
  | NewRatings({
        new_player1 = {rating = p1rating;};
        new_player2 = {rating = p2rating;};
    }) ->
     Alcotest.check
       (Alcotest.float 1e-2)
       "ratings should be larger"
       1687.65
       p1rating;

     Alcotest.check
       (Alcotest.float 1e-2)
       "rating should be lower"
       1025.55
       p2rating
  | _ -> Alcotest.fail "should return new ratings"

let test_deviation_is_updated () =
  let game_result =
    {
      player1 = default_player ();
      player2 = default_player ();
      game_outcome = Player1Win;
    } in
  match rate_single_game game_result with
  | NewRatings({
        new_player1 = {rating_deviation = p1deviation;};
        new_player2 = {rating_deviation = p2deviation;};
    }) ->
     Alcotest.check
       (Alcotest.float 1e-2)
       "ratings should be larger"
       290.32
       p1deviation;

     Alcotest.check
       (Alcotest.float 1e-2)
       "rating should be lower"
       290.32
       p2deviation
  | _ -> Alcotest.fail "should return new ratings"

let test_deviation_is_updated2 () =
  let game_result =
    {
      player1 = {(default_player ()) with rating = 2500.};
      player2 = default_player ();
      game_outcome = Player1Win;
    } in
  match rate_single_game game_result with
  | NewRatings({
        new_player1 = {rating_deviation = p1deviation;};
        new_player2 = {rating_deviation = p2deviation;};
    }) ->
     Alcotest.check
       (Alcotest.float 1e-2)
       "ratings should be larger"
       343.84
       p1deviation;

     Alcotest.check
       (Alcotest.float 1e-2)
       "rating should be lower"
       343.84
       p2deviation
  | _ -> Alcotest.fail "should return new ratings"

let test_deviation_is_updated3 () =
  let game_result =
    {
      player1 = {(default_player ()) with rating = 2500.};
      player2 = default_player ();
      game_outcome = Player2Win;
    } in
  match rate_single_game game_result with
  | NewRatings({
        new_player1 = {rating_deviation = p1deviation;};
        new_player2 = {rating_deviation = p2deviation;};
    }) ->
     Alcotest.check
       (Alcotest.float 1e-2)
       ""
       343.84
       p1deviation;

     Alcotest.check
       (Alcotest.float 1e-2)
       ""
       343.84
       p2deviation
  | _ -> Alcotest.fail "should return new ratings"

let test_deviation_is_updated4 () =
  let game_result =
    {
      player1 = {(default_player ())
                with volatility = 0.0001};
      player2 = default_player ();
      game_outcome = Player1Win;
    } in
  match rate_single_game game_result with
  | NewRatings({
        new_player1 = {rating_deviation = p1deviation;};
        new_player2 = {rating_deviation = p2deviation;};
    }) ->
     Alcotest.check
       (Alcotest.float 1e-2)
       ""
       290.23
       p1deviation;

     Alcotest.check
       (Alcotest.float 1e-2)
       ""
       290.32
       p2deviation
  | _ -> Alcotest.fail "should return new ratings"

let test_rating_with_volatility1 () =
  let game_result =
    {
      player1 = {(default_player ())
                with volatility = 0.0001};
      player2 = {(default_player ())
                with volatility = 1.};
      game_outcome = Player1Win;
    } in
  match rate_single_game game_result with
  | NewRatings({
        new_player1 = {rating = p1rating;};
        new_player2 = {rating = p2rating;};
    }) ->
     Alcotest.check
       (Alcotest.float 1e-2)
       ""
       1662.21
       p1rating;

     Alcotest.check
       (Alcotest.float 1e-2)
       ""
       1312.35
       p2rating
  | _ -> Alcotest.fail "should return new ratings"

let test_rating_with_volatility2 () =
  let game_result =
    {
      player1 = {(default_player ())
                with volatility = 0.0001;
                     rating_deviation = 10.};
      player2 = {(default_player ())
                with volatility = 1.;
                     rating_deviation = 10.};
      game_outcome = Player1Win;
    } in
  match rate_single_game game_result with
  | NewRatings({
        new_player1 = {rating = p1rating;};
        new_player2 = {rating = p2rating;};
    }) ->
     Alcotest.check
       (Alcotest.float 1e-2)
       ""
       1500.29
       p1rating;

     Alcotest.check
       (Alcotest.float 1e-2)
       ""
       1430.62
       p2rating
  | _ -> Alcotest.fail "should return new ratings"

let rate_suite = [
    "rate rates", `Quick, test_simple_rate;
    "draw rates correctly", `Quick, test_simple_rate2;
    "win changes rating", `Quick, test_win_changes_rating;
    "rating deviation affects new ratings", `Quick, test_deviation_matters;
    "volatility affects new ratings", `Quick, test_volatility_matters;
    "rating deviation is updated", `Quick, test_deviation_is_updated;
    "irrelevant result doesn't change deviation much",
    `Quick, test_deviation_is_updated2;
    "big upset doesn't change deviation much",
    `Quick, test_deviation_is_updated3;
    "volatility affects deviation",
    `Quick, test_deviation_is_updated4;
    "volatility affects rating", `Quick, test_rating_with_volatility1;
    "volatility affects rating", `Quick, test_rating_with_volatility2;
  ]

let () =
  Alcotest.run
    "glicko2 lib"
    [
      "default player", default_player_suite;
      "rate", rate_suite;
    ]
