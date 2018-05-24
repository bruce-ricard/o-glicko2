let tau = function
  | `Default -> 0.5
  | `High -> 1.2
  | `Low -> 0.3
  | `Manual v -> v

let epsilon = function
  | `Default -> 1e-6
  | `Manual v -> v

let max_iterations = function
  | `Default -> 100
  | `Manual i -> i

let rating = function
  | `Default -> 1500
  | `Manual i -> i

let deviation = function
  | `Default -> 350
  | `Manual i -> i

let volatility = function
  | `Default -> 0.06
  | `Manual f -> f
