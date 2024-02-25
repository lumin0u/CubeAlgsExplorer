open Algorithm
open CuberProfile

(* time is float, locally *)
open struct type time = float end

(* si l = q @ l' où = est l'égalité sémantique, eat_opt l q s'évalue en l' *)
(* s'évalue en None sinon *)
let rec eat_opt (l: e_alg) (q: e_alg): e_alg option =
  match l, q with
  | l, [] -> Some l
  | [], _ -> None
  | (m, n)::l, (m', n')::q ->
    if m <> m' then None
    else if n = n' then eat_opt l q
    else if n < n' then eat_opt l ((m', n'-n)::q)
    else eat_opt ((m, n-n')::l) q

let is_regrip (alg: alg): bool =
  match alg with
  | [] | [('x', _)] | [('y', _)] | [('z', _)] -> true
  | _ -> false


let compare_e_move (m1: e_move) (m2: e_move): int =
  match m1, m2 with
  | U, D | R, L | F, B -> 1
  | D, U | L, R | B, F -> -1
  | _ -> 0

(*  multiply_cubes (inverse_cube target) cube  *)
(** will try to compute 'count' maneuvers, may return less *)
let fastest_maneuvers
    ?(timeout = 0.0)
    ?(heuristic = 0.1)
    (count: int)
    (move_times: (handed_move, float) Hashtbl.t)
    (cube: cube)
    : (time * maneuver) list =
  let move_times =
    move_times
    |> Hashtbl.to_seq
    |> Seq.filter_map (fun ((from_, alg, to_), t) ->
      if t <= 0.0 then None else
        Some (from_, alg, to_,
        t +. 0., t (*(if from_ = to_ || (match alg with | ('R', _)::_ | ('L', _)::_ -> true | _ -> false) then 0. else 0.05) +. (if (let (x, y) = to_ in x = Out || y = Out) then 0.0 else 0.)*)
        )
      )
    |> List.of_seq
  in
  let cc, init_dist = OptSolver.cc_dist_of cube in

  (* on peut en faire des choses avec un cube en 5 secondes *)
  let cost (t, _, cube, (cc, max_dist), _, _, _) =
    t +. heuristic *. float_of_int max_dist
  in
  let frontier = ref (
    cross [F] [F; U; D; Bu; Bd; M] @ cross [F; U; D; Bu; Bd; M] [F]
    |> List.map (fun x -> (0.0, x, cube, (cc, init_dist), 0.0, []))
    |> cross_f (fun h (t, hp, cube, cc, t0, hist) -> (t, hp, cube, cc, t0, hist, h)) [R; L]
    |> List.fold_left (fun acc x -> PrioQueue.insert acc (cost x) x) PrioQueue.empty
    )
  and shortests = Array.make count None in
  let counter = ref (count - 1) in

  (*let is_greater_than_shortest (t: time): bool =
    match !shortest with
    | None -> false
    | Some (ts, _, _, _, _, _, _) -> t >= ts
  in*)
  let start_date = if timeout > 0.0 then Sys.time () else 0.0 in
  while (!frontier <> Empty && !counter >= 0) && (timeout = 0.0 || Sys.time () -. start_date < timeout) do
    let my_cost, nearest, nfrontier = PrioQueue.extract !frontier in
    let (time, hand_pos, cube, (cc, max_dist), time0, hist, hand) = nearest in
    frontier := nfrontier;
    if is_solved_cube cube then
      shortests.(decr counter; !counter + 1) <- Some nearest
    else
      move_times
      |> List.filter_map (fun (h1, m, h2, t, t0) ->
        let obv_bad =
          match hist with
          | (_, last_move, _)::_ -> is_regrip m && is_regrip last_move
          | _ -> is_regrip m
        in
        if h1 <> hand_pos || obv_bad (*|| is_greater_than_shortest (t +. time)*) then None
        else
          let (hl1, hr1), (hl2, hr2) = h1, h2 in
          let nt = t +. (if (
            match hand with
            | L -> hl1 <> hl2
            | R -> hr1 <> hr2
            ) then 0.2 else 0.0
          )
           +. (if List.for_all (fun (m, _) -> List.exists (fun (_, a, _) -> List.exists (fun (m', _) -> m=m') a) hist) m then 0.0 else heuristic)
          in
          let sent_alg = 
            elementalize m (let _,r = cube in !r)
            |> fst
            |> List.stable_sort (fun (m1, _) (m2, _) -> compare_e_move m1 m2)
          in
          let cc, max_dist = List.fold_left (fun (cc, _) m ->
              OptSolver.next cc m
            ) (cc, max_dist) sent_alg
          in
          Some (m, h2, nt, t0, apply_alg cube m, cc, max_dist)
      )
      |> List.iter (fun (m, h2, t, t0, cube, cc, max_dist) ->
        let me = (t +. time, h2, cube, (cc, max_dist), t0 +. time0, (hand_pos, m, h2)::hist, hand) in
        frontier := PrioQueue.insert !frontier (cost me) me
      )
  done;
  Array.fold_left (fun acc -> function
  | None -> acc
  | Some (_, h_end, _, _, t0, hist, _) -> (t0, List.rev hist)::acc)
  [] shortests

let fastest_maneuver_opt
    ?(timeout = 0.0)
    ?(heuristic = 0.1)
    (move_times: (handed_move, float) Hashtbl.t)
    (cube: cube)
    : (time * maneuver) option =
  match fastest_maneuvers ~timeout ~heuristic 1 move_times cube with
  | [] -> None
  | x::_ -> Some x


let fastest_maneuver
    ?(timeout = 0.0)
    ?(heuristic = 0.1)
    (move_times: (handed_move, float) Hashtbl.t)
    (cube: cube)
    : time * maneuver =
  match fastest_maneuvers ~timeout ~heuristic 1 move_times cube with
  | [] -> failwith "erreur erreur erreur (AlgFinderElem.fastest_maneuver)"
  | x::_ -> x
