module Config =
  struct
    (* To update those 3 values, you should read the Glicko2
       paper: http://www.glicko.net/glicko/glicko2.pdf *)

    let tau = `High

    (* Unless you have strong performance requirements, you
       probably should leave this to `Default *)
    let epsilon = `Manual 1e-4

    (* Unless you have strong performance requirements, you
       probably should leave this to `Default *)
    let max_volatility_iterations = `Manual 50


    (* These 3 last values let you set the
       default player's settings *)

    let default_rating = `Manual 1200
    let default_deviation = `Manual 215

    (* You should read the Glicko2 paper before updating
        this value *)
    let default_volatility = `Default
  end

module M = Glicko2.Make(Config)
module Glicko2 = M.SingleGame

(* Now you can use the Glicko2 module like you would use it
in simple.ml or low_level.ml *)
