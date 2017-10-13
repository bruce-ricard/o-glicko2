type player =
  {
    rating: float;
    rating_deviation: float;
    sigma: float;
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

val rate: game_result -> rate_result
