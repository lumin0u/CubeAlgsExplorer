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

(** will try to compute 'count' maneuvers, may return less *)
let fastest_maneuvers
    ?(timeout = 0.0)
    ?(heuristic = 0.1)
    (count: int)
    (move_times: (handed_move, float) Hashtbl.t)
    (alg: e_alg)
    (start_rot: cube_rot)
    (final_rot: cube_rot)
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

  (* assigned to a hand position are the moves possible accessible from it *)
  let move_times_dict = Hashtbl.create 64 in
  cross [F; U; D; Bu; Bd; M; Af; Out] [F; U; D; Bu; Bd; M; Af; Out]
  |> cross [R; L]
  |> List.iter (fun (hand, hand_pos) ->
    Hashtbl.add move_times_dict (hand, hand_pos)
    (move_times
    |> List.filter_map (fun (h1, m, h2, t, t0) ->
      if h1 <> hand_pos then None
      else
        let (hl1, hr1), (hl2, hr2) = h1, h2 in
        let nt = t +. (if (
          match hand with
          | L -> hl1 <> hl2
          | R -> hr1 <> hr2
          ) then 0.2 else 0.0
        )
        in
        Some (m, h2, nt +. (if is_regrip m then 0.05 else 0.0), t0)
    ))
  );

  (* on peut en faire des choses avec un cube en 5 secondes *)
  let cost (t, _, alg, _, _, _, _, _) =
    t +. ((List.fold_left (fun (acc, l) (m, _) ->
      match l with
      | Some l -> if e_is_parallel m l then (acc, None) else (acc +. heuristic, Some m)
      | None -> (acc +. heuristic, Some m)
      ) (0.0, None) alg) |> fst) (* goodbye heuristic function ! *)
  in
  let frontier = Heap.create () in
  cross [F] [F; U; D; Bu; Bd; M] @ cross [F; U; D; Bu; Bd; M] [F]
  |> List.map (fun x -> (0.0, x, alg, start_rot, 0.0, [], []))
  |> cross_f (fun h (a, b, c, d, e, f, g) -> (a, b, c, d, e, f, h, g)) [R; L]
  |> List.iter (fun x -> Heap.add (cost x) x frontier);

  let shortests = Array.make count None in
  let counter = ref (count - 1) in

  (*let is_greater_than_shortest (t: time): bool =
    match !shortest with
    | None -> false
    | Some (ts, _, _, _, _, _, _) -> t >= ts
  in*)
  let start_date = if timeout > 0.0 then Sys.time () else 0.0 in
  while (Heap.is_empty frontier |> not && !counter >= 0) && (timeout = 0.0 || Sys.time () -. start_date < timeout) do
    let _, nearest = Heap.take_min frontier in
    let (time, hand_pos, alg, cube_rot, time0, hist, hand, used_m) = nearest in
    if alg = id_alg && cube_rot = final_rot then
      shortests.(decr counter; !counter + 1) <- Some nearest
    else
      Hashtbl.find move_times_dict (hand, hand_pos)
      |> List.iter (fun (m, h2, t, t0) ->
        let obv_bad =
          match hist with
          | (_, last_move, _)::_ -> is_regrip m && is_regrip last_move
          | _ -> is_regrip m
        in
        if not obv_bad then
          let new_moves = List.filter_map (fun (m, _) -> if List.mem m used_m then None else Some m) m in
          let nt = t +. if new_moves = [] then 0.0 else 0.5 *. heuristic
          in
          let e_m, cr = elementalize m cube_rot in
          match eat_opt alg e_m with
          | Some alg ->
            let me = (nt +. time, h2, alg, cr, t0 +. time0, (hand_pos, m, h2)::hist, hand, new_moves @ used_m) in
            Heap.add (cost me) me frontier
          | None -> ()
      )
  done;
  Array.fold_left (fun acc -> function
  | None -> acc
  | Some (_, h_end, _, _, t0, hist, _, _) -> (t0, List.rev hist)::acc)
  [] shortests

let fastest_maneuver_opt
    ?(timeout = 0.0)
    ?(heuristic = 0.1)
    (move_times: (handed_move, float) Hashtbl.t)
    (alg: e_alg)
    (start_rot: cube_rot)
    (final_rot: cube_rot)
    : (time * maneuver) option =
  match fastest_maneuvers ~timeout ~heuristic 1 move_times alg start_rot final_rot with
  | [] -> None
  | x::_ -> Some x

let fastest_maneuver
    ?(heuristic = 0.1)
    (move_times: (handed_move, float) Hashtbl.t)
    (alg: e_alg) (start_rot: cube_rot)
    (final_rot: cube_rot)
    : time * maneuver =
  match fastest_maneuver_opt ~heuristic move_times alg start_rot final_rot with
  | None -> failwith "erreur erreur erreur (AlgEvaluatorElem.fastest_maneuver)"
  | Some x -> x
