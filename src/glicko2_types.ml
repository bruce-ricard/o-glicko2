module type GLICKO2_CONFIG =
  sig
    val tau : [ `Default | `High | `Low | `Manual of float ]
    val epsilon : [ `Default | `Manual of float ]
    val max_volatility_iterations : [ `Default | `Manual of int ]

    val default_rating : [ `Default | `Manual of int ]
    val default_deviation : [ `Default | `Manual of int ]
    val default_volatility :  [ `Default | `Manual of float ]
  end

module type GLICKO2 =
  sig
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

    type unknown_error = [
        `UnknownError of string
      ]

    type rate_error =
      [
      | `InvalidVolatility
      | `ExceededIterations
      | unknown_error
      ]

    type player_error = [
      | `InvalidArgument of string
      | unknown_error
      ]

    type 'a rate_result = ('a, rate_error)  Core.Std.Result.t
    type player_result = (player, player_error) Core.Std.Result.t

    module LowLevel :
    sig
      type personal_result = [ `Win | `Lose | `Draw ]

      type opponent =
        {
          rating: float;
          rating_deviation: float;
        }

      val player_to_opponent : player -> opponent

      type game_outcome = {
          opponent: opponent;
          result: personal_result;
        }

      type game_results =
        {
          player : player;
          games : game_outcome * game_outcome list
        }

      val default_player:
        ?rating:int -> ?rating_deviation:int -> unit
        -> player_result
      val rate: game_results -> player rate_result
    end

    module SingleGame :
    sig
      type game_outcome =
        [ `Player1Win | `Player2Win | `Draw ]

      type new_ratings =
        {
          new_player1: player;
          new_player2: player;
        }

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

      (**
    The recommended way of creating a new player.
    It will have a rating of 1500 (unless overwritten),
    a rating deviation of 350 (unless overwritten), and
    a volatility of 0.06.
       *)
      val default_player:
        ?rating:int -> ?rating_deviation:int -> unit
        -> player_result

      val rate: game_result -> new_ratings rate_result
      val update_player_after_not_player_in_rating_period:
        player -> player_result
    end
  end
