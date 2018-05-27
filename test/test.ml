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
      "special config", Test_special_config.suite;
    ]
