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

type rate_result =
  {
    new_player1: player;
    new_player2: player;
  }

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

let rate game_result =
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
