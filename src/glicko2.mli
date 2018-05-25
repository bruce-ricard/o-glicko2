open Glicko2_types

(** The pre-instanciated, ready to use Glicko2
    module. *)
module Default : GLICKO2

(** Functor returning a Glicko2 module, allowing you
    to configure certain variables of the algorithm.
 *)
module Make : functor (Config : GLICKO2_CONFIG) -> GLICKO2

(** The log source used in the library*)
val log_source : Logs.src
