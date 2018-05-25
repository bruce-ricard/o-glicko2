open Glicko2_types

module Default : GLICKO2
module Make : functor (Config : GLICKO2_CONFIG) -> GLICKO2

val log_source : Logs.src
