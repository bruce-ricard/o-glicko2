type player =
  {
    rating: float;
    rating_deviation: float;
    volatility: float;
  }

type player_return =
  | Player of player
  | Error of string

module Player =
  struct
    let default_player ?(rating=1500) ?(rating_deviation=350.) () =
      if rating < 100 then
        Error "rating cannot be lower than 100"
      else if rating_deviation < 0. then
        Error "rating_deviation cannot be negative"
      else if rating_deviation > 350. then
        Error "rating_deviation cannot be greater than 350"
      else
        Player
          {
            rating = float_of_int rating;
            rating_deviation;
            volatility = 0.06;
          }
  end


let player_to_string player =
  Printf.sprintf
    "{rating = %f; rating_deviation = %f; volatility = %f}"
    player.rating
    player.rating_deviation
    player.volatility

let list_to_string ts elts =
  let rec aux = function
    | [] -> ""
    | [x] -> ts x
    | x :: xs -> ts x ^ "; " ^ aux xs
  in
  "[ " ^ aux elts ^ " ]"

(*
let game_results_to_string result =
  let games =
    let g,gs = result.games in
    g :: gs in
  Printf.sprintf
    "{player = %s; games = %s}"
    (player_to_string result.player)
    (list_to_string outcome_to_string games)
 *)

let internal_player player =
  let open Glicko_internal in
  {
    r = player.rating;
    rd = player.rating_deviation;
    sigma = player.volatility;
  }

let internal_opponent player game_result =
  let open Glicko_internal in
  { rj = player.rating;
    rdj = player.rating_deviation;
    sj = game_result;
  }

let player_from_internal internal =
  let open Glicko_internal in
  {
    rating = internal.r;
    rating_deviation = internal.rd;
    volatility = internal.sigma;
  }

let internal_outcome =
  let open Glicko_internal in
  function
  | `Win -> Win
  | `Lose -> Lost
  | `Draw -> Draw


let is_too_small volatility = volatility < 1e-10

module LowLevel =
  struct
    include Player
    type personal_result = [ `Win | `Lose | `Draw ]

    type opponent =
      {
        rating: float;
        rating_deviation: float;
      }


    let opponent_to_internal opponent game_result =
      let open Glicko_internal in
      { rj = opponent.rating;
        rdj = opponent.rating_deviation;
        sj = game_result }


    type game_outcome = {
        opponent: opponent;
        result: personal_result;
      }

    type game_results =
      {
        player: player;
        games : game_outcome * game_outcome list
      }


    let rate_unsafes game_results =
      let player = internal_player game_results.player in
      let game_outcomes =
        let g,gs = game_results.games in
        g :: gs  in

      let new_player =
        Glicko_internal.rate
          player
          (List.map
             (function game_outcome ->
                       opponent_to_internal
                         game_outcome.opponent
                         (internal_outcome game_outcome.result)
             )
             game_outcomes
          )
      in
      Player (player_from_internal new_player)

    let rate game_results =
      if is_too_small game_results.player.volatility then
        begin
          Logs.err (fun m ->
              m
                "Invalid volatlity on input: %s"
                (player_to_string game_results.player)
            );
          Error "IV"(* InvalidVolatility*)
        end
      else
        try
          rate_unsafes game_results
        with
        | Glicko_internal.Exceeded_Iterations ->
           Logs.err (fun m ->
               m "Glicko2 Exceeded iterations"
             ); Error "IT Glicko2 exceeded iterations"
        | e ->
           Logs.err (fun m ->
               m "Glicko2 unknown error %s"
                 (Printexc.to_string e)
             ); Error (Printexc.to_string e)
  end

module SingleGame =
  struct
    include Player


    type game_outcome =
    [ `Player1Win | `Player2Win | `Draw ]

    type game_result =
      {
        player1: player;
        player2: player;
        game_outcome: game_outcome
      }

    type players = P1 | P2

    type new_ratings =
      {
        new_player1: player;
        new_player2: player;
      }

    type rate_result =
      | NewRatings of new_ratings
      | InvalidVolatility
      | InternalError of string

    let outcome_to_string = function
      | `Player1Win -> "Player1Win"
      | `Player2Win -> "Player2Win"
      | `Draw -> "Draw"

    let game_result_to_string result =
      Printf.sprintf
        "{player1 = %s; player2 = %s; game_outcome = %s}"
        (player_to_string result.player1)
        (player_to_string result.player2)
        (outcome_to_string result.game_outcome)


    let personal_game_outcome game_outcome player =
      let open Glicko_internal in
      match game_outcome,player with
      | `Player1Win, P1 | `Player2Win, P2 -> Win
      | `Draw, _ -> Draw
      | `Player1Win, P2 | `Player2Win, P1 -> Lost

    let rate_unsafe game_result =
      let p1 = internal_player game_result.player1
      and p2 = internal_player game_result.player2 in

      let newp1 = Glicko_internal.rate
                    p1
                    [internal_opponent
                       game_result.player2
                       (personal_game_outcome game_result.game_outcome P1)
                    ]
      and newp2 = Glicko_internal.rate
                    p2
                    [internal_opponent
                       game_result.player1
                       (personal_game_outcome game_result.game_outcome P2)
                    ]
      in
      {
        new_player1=player_from_internal newp1;
        new_player2=player_from_internal newp2
      }

    let rate game_result =
      if is_too_small game_result.player1.volatility
         || is_too_small game_result.player2.volatility then
        begin
          Logs.err (fun m ->
              m
                "Invalid volatlity on input: %s"
                (game_result_to_string game_result)
            );
          InvalidVolatility
        end
      else
        try
          NewRatings (rate_unsafe game_result)
        with
        | Glicko_internal.Exceeded_Iterations ->
           Logs.err (fun m ->
               m
                 "Glicko2 Exceeded iterations on input: %s"
                 (game_result_to_string game_result)
             ); InternalError "Glicko2 exceeded iterations"
        | e ->
           Logs.err (fun m ->
               m
                 "Glicko2 unknown error on input %s: %s"
                 (game_result_to_string game_result)
                 (Printexc.to_string e)
             ); InternalError (Printexc.to_string e)

    let update_player_after_not_player_in_rating_period player =
      let internal_p = internal_player player in
      let updated_internal =
        Glicko_internal.update_after_not_playing_in_rating_period
          internal_p in
      Player (player_from_internal updated_internal)

  end