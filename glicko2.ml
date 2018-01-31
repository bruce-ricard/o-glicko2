type player =
  {
    rating: float;
    rating_deviation: float;
    volatility: float;
  }

type game_outcome =
  Player1Win | Player2Win | Draw

type game_result =
  {
    player1: player;
    player2: player;
    game_outcome: game_outcome
  }

let game_result_to_string result =
  let player_to_string player =
    Printf.sprintf
      "{rating = %f; rating_deviation = %f; volatility = %f}"
      player.rating
      player.rating_deviation
      player.volatility
  in
  let outcome_to_string = function
    | Player1Win -> "Player1Win"
    | Player2Win -> "Player2Win"
    | Draw -> "Draw"
  in
  Printf.sprintf
    "{player1 = %s; player2 = %s; game_outcome = %s}"
    (player_to_string result.player1)
    (player_to_string result.player2)
    (outcome_to_string result.game_outcome)

type rate_result =
  {
    new_player1: player;
    new_player2: player;
  }

type result =
  | NewRatings of rate_result
  | InvalidVolatility
  | InternalError

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
    sj = game_result }

let player_from_internal internal =
  let open Glicko_internal in
  {
    rating = internal.r;
    rating_deviation = internal.rd;
    volatility = internal.sigma
  }

type players = P1 | P2

let personal_game_outcome game_outcome player =
  let open Glicko_internal in
  match game_outcome,player with
  | Player1Win, P1 | Player2Win, P2 -> Win
  | Draw, _ -> Draw
  | Player1Win, P2 | Player2Win, P1 -> Lost

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

let is_too_small volatility = volatility < 1e-10

let rate game_result =
  if is_too_small game_result.player1.volatility
     || is_too_small game_result.player2.volatility then
    InvalidVolatility
  else
    try
      NewRatings (rate_unsafe game_result)
    with
    | Glicko_internal.Exceeded_Iterations ->
       Logs.err (fun m ->
           m
             "Glicko2 Exceeded iterations on input: %s"
             (game_result_to_string game_result)
         ); InternalError
    | e ->
       Logs.err (fun m ->
           m
             "Glicko2 unknown error on input %s: %s"
             (game_result_to_string game_result)
             (Printexc.to_string e)
         ); InternalError
