(* Types *)
type player =
	{ r : float;
	  rd : float;
	  sigma : float }

type opponent =
	{ rj : float;
	  rdj : float;
	  sj : float }

let print_opponent (muj, phij, gphij, emmp, sj) =
	Printf.printf "Muj: %f; PhiJ: %f; GPhij: %f; EMMP: %f; Sj: %f\n" muj phij gphij emmp sj

let printf_opponents opps = List.iter print_opponent opps

(* Constants *)
let pi = 3.14159265
let tau = 0.5
let epsilon = 0.000001

let scale_factor = 173.7178

(* Helper functions *)
let square x = x *. x
let sum xs = List.fold_left (+.) 0.0 xs

let g phi =
	1.0 /. sqrt (1.0 +. 3.0 *. phi *. phi /. (pi *. pi))

let e mu muj phij =
	1.0 /. (1.0 +. (exp (-. g(phij) *. (mu -. muj))))

(* Step 2 *)
let scale r rd =
	let mu = (r -. 1500.0) /. scale_factor in
	let phi = rd /. scale_factor in
	  (mu, phi)
	 
let scale_opponents mu xs =
  List.map
    (function {rj; rdj; sj} ->
      let (muj, phij) = scale rj rdj in
        (muj, phij, g phij, e mu muj phij, sj)) xs

(* Step 3 *)
let update_rating os =
    1.0 /. sum ( List.map (function (_, _, gphij, emmp, _) -> square gphij *. emmp *. (1.0 -. emmp)) os )
    
(* Step 4: Delta computation *)
let compute_delta v opps =
	let f (_, _, gphij, emmp, sj) = gphij *. (sj -. emmp) in
	  v *. sum (List.map f opps)
    
(* Step 5: Volatility computation *)
let vol_f phi v delta a =
	let phi2 = phi *. phi in
	  function x ->
	    let ex = exp x in
	    let d2 = delta *. delta in
	    let a2 = phi2 +. v +. ex in
	    let p2 = (x -. a) /. (tau *. tau) in
	    let p1 = (ex *. (d2 -. phi2 -. v -. ex)) /. (2.0 *. a2 *. a2) in
	      p1 -. p2
	    
let rec vol_k k f a =
	let const = a -. (float_of_int k) *. sqrt (tau *. tau) in
	if (f const) < 0.0 then
		vol_k (k + 1) f a
	else
		const

let compute_volatility sigma phi v delta =
	let rec loop a b f fa fb =
		if abs_float (b -. a) < epsilon then
			exp (a /. 2.0)
		else
			let c = a +. (a -. b) *. fa /. (fb -. fa) in
			let fc = f c in
			let (na, nfa) =
				if fc *. fb < 0.0 then
					(b, fb)
				else
					(a, fa /. 2.0) in
			loop na c f nfa fc in
	let a = log (sigma *. sigma) in
	let f = vol_f phi v delta a in
	let b =
		if delta *. delta > phi *. phi +. v then
			log (delta *. delta -. phi *. phi -. v)
		else
			vol_k 1 f a in
	let fa = f a in
	let fb = f b in
	  loop a b f fa fb

(* Step 6 *)
let phi_star sp phi = sqrt ( square phi +. square sp )

(* Step 7 *)
let new_rating phistar mu v opps =
	let phip = 1.0 /. sqrt ( (1.0 /. square phistar) +. (1.0 /. v) ) in
	let l = List.map (function (_, _, gphij, emmp, sj) -> gphij *. (sj -. emmp)) opps in
	let mup = mu +. square phip *. sum l in
	(mup, phip)

(* Step 8 *)
let unscale mup phip =
	let rp = scale_factor *. mup +. 1500.0 in
	let rdp = scale_factor *. phip in
	(rp, rdp)
	
let rate {r; rd; sigma} opps =
	let (mu, phi) = scale r rd in
	let sopps = scale_opponents mu opps in
	let v = update_rating sopps in
	let delta = compute_delta v sopps in
	let sigmap = compute_volatility sigma phi v delta in
	let phistar = phi_star sigmap phi in
	let (mup, phip) = new_rating phistar mu v sopps in
	let (r1, rd1) = unscale mup phip in
	{r = r1; rd = rd1; sigma = sigmap}

let bench () =
	let p = {r = 1500.0; rd = 200.0; sigma = 0.06} in
	let opps =
		[{rj = 1400.0; rdj = 30.0; sj = 1.0};
		 {rj = 1550.0; rdj = 100.0; sj = 0.0};
		 {rj = 1700.0; rdj = 300.0; sj = 0.0}] in
	let loop = function
		| 0 -> ()
		| n ->
			rate p opps;
			loop (n-1) in
		loop

(* Simple Test *)
let _ =
	let r = 1500.0 in
	let rd = 200.0 in
	let sigma = 0.06 in
	let opps =
		[{rj = 1400.0; rdj = 30.0; sj = 1.0};
		 {rj = 1550.0; rdj = 100.0; sj = 0.0};
		 {rj = 1700.0; rdj = 300.0; sj = 0.0}] in
	let (0.0, 1.1512924985234674) = scale r rd in
	let (mu, phi) = scale r rd in
	let sopps = scale_opponents mu opps in
	let v = update_rating sopps in
	let delta = compute_delta v sopps in
	let sigmap = compute_volatility sigma phi v delta in
	let phistar = phi_star sigmap phi in
	let (mup, phip) = new_rating phistar mu v sopps in
	let (r1, rd1) = unscale mup phip in
	  Printf.printf "Scaling step: %f %f\n" mu phi;
	  printf_opponents sopps;
	  Printf.printf "V: %f\n" v;
	  Printf.printf "Delta: %f\n" delta;
	  Printf.printf "SigmaP: %f\n" sigmap;
	  Printf.printf "Phi Star: %f\n" phistar;
	  Printf.printf "MuP: %f; PhiP: %f\n" mup phip;
	  Printf.printf "R1: %f; RD1: %f\n" r1 rd1


	  
