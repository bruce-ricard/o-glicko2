(*
 * Glicko2 library. Implements the Glicko2 algorithm.
 * Copyright (C) 2018-present Bruce Ricard
 *
 * This file is part of glicko2.
 *
 * Glicko2 is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
 * (at your option) any later version.
 *
 * Glicko2 is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Glicko2. If not, see <http://www.gnu.org/licenses/>.
 *)


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
