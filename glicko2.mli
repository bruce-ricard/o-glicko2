(**
The type containing the data need for the Glicko2 algorithm.
You can create this structure yourself, but we recommend you
use the "default_player" function defined in this module.
 *)
type player =
  {
    rating: float;
    rating_deviation: float;
    volatility: float;
  }

(* TODO make this a polymorphic variant to be able to subtype it if needed. Or not? *)
(**
 *)
type game_outcome =
  Player1Win | Player2Win | Draw

(**
The input type to the Glicko2 rating functions.
The two players, and the outcome of the game.
 *)

type game_result =
  {
    player1: player;
    player2: player;
    game_outcome: game_outcome;
  }

type personal_result = [ `Win | `Lose | `Draw ]

type opponent =
  {
    o_rating: float;
    o_rating_deviation: float;
  }

type g = {
    opponent: opponent;
    result: personal_result;
  }

type multiple_games_result =
  {
    player: player;
    games : g * g list
  }

type new_ratings =
  {
    new_player1: player;
    new_player2: player;
  }

type rate_result =
  | NewRatings of new_ratings
  | InvalidVolatility
  | InternalError of string

type player_return =
  | Player of player
  | Error of string

(**
The recommended way of creating a new player.
It will have a rating of 1500 (unless overwritten),
a rating deviation of 350 (unless overwritten), and
a volatility of 0.06.
 *)
val default_player:
  ?rating:int -> ?rating_deviation:float -> unit -> player_return

val rate: multiple_games_result -> player_return

val rate_single_game: game_result -> rate_result

val update_player_after_not_player_in_rating_period:
  player -> player_return
