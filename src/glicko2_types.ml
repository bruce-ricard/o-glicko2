module type GLICKO2_CONFIG =
  sig
    (** To update those 3 values, you should read the Glicko2
       paper: http://www.glicko.net/glicko/glicko2.pdf *)

    (** `High : 1.2, `Default : 0.5, `Low : 0.3 *)
    val tau : [ `Default | `High | `Low | `Manual of float ]

    (** Unless you have strong performance requirements, you
       probably should leave this to `Default *)
    val epsilon : [ `Default | `Manual of float ]

    (** Unless you have strong performance requirements, you
       probably should leave this to `Default *)
    val max_volatility_iterations : [ `Default | `Manual of int ]


    (** These 3 last values let you set the
       default player's settings *)
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


    (** This module contains a rate function very close from the one
        described in the paper. You should feed it a list of games
        played by a player during a rating period, and it returns
        the new rating of that player. You should run the functions
        in this module only at the end of your rating period.
     *)
    module LowLevel :
    sig
      type personal_result = [ `Win | `Lose | `Draw ]

      type opponent =
        {
          rating: float;
          rating_deviation: float;
        }

      (** utility function converting a player into an opponent,
          needed by the rate function *)
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
      val update_player_after_not_player_in_rating_period:
        player -> player_result
    end

    (** This module contains a simple API to rate players who
        play games. After each game, call the rate function
        with the players who played it and the result of the
        game. It will output the new ratings of both players.

        It doesn't *really* follow the paper in the sense that
        the glicko algorithm is supposed to be used within a
        "rating period", at the end of which you should rate all
        the games from this period (see LowLevel module if you want
        that), but it's good enough to start if you just have a
        little website with people playing games who want ratings.
     *)
    module SingleGame :
    sig
      type game_outcome =
        [ `Player1Win | `Player2Win | `Draw ]

      type new_ratings =
        {
          new_player1: player;
          new_player2: player;
        }

      (** The input type to the Glicko2 rating functions.
          The two players, and the outcome of the game.
       *)
      type game_result =
        {
          player1: player;
          player2: player;
          game_outcome: game_outcome;
        }

      (** The recommended way of creating a new player.
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
