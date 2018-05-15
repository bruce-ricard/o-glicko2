open Glicko2

let () = Logs.set_reporter (Logs_fmt.reporter ())

let () =
  Alcotest.run
    "glicko2 lib"
    [
      "default player", Test_default_player.default_player_suite;
      "rate single game", Test_rate_single_game.rate_single_game_suite;
      "rate", Test_low_level.rate_suite;
      "update no game", Test_update_no_game.update_suite;
    ]
