open Algorithm
open CuberProfile

(* time is float, locally *)
open struct type time = float end

(* si l = q @ l' où = est l'égalité sémantique, eat_opt l q s'évalue en l' *)
(* s'évalue en None sinon *)
let rec eat_opt (l: alg) (q: alg): alg option =
  match l, q with
  | l, [] -> Some l
  | [], _ -> None
  | (m, n)::l, (m', n')::q ->
    if m <> m' then None
    else if n = n' then eat_opt l q
    else if n < n' then eat_opt l ((m', n'-n)::q)
    else eat_opt ((m, n-n')::l) q

let fastest_maneuver_opt ?(timeout = 0.0) (move_times: (handed_move, float) Hashtbl.t) (alg: alg): (time * maneuver) option =
  let move_times =
    move_times
    |> Hashtbl.to_seq
    |> Seq.filter_map (fun ((from_, alg, to_), t) ->
      if t <= 0.0 then None else
        Some (from_, alg, to_,
        t +. (if from_ = to_ || (match alg with | ('R', _)::_ | ('L', _)::_ -> true | _ -> false) then 0. else 0.05) +. (if (let (x, y) = to_ in x = Out || y = Out) then 0.0 else 0.), t
        )
      )
    |> List.of_seq
  in

  let time_min_each = Hashtbl.create (List.length all_moves) in
  List.iter (fun (_, ml, _, t, _) ->
    match ml with
    | [m] ->
      (match Hashtbl.find_opt time_min_each m with
        | None -> Hashtbl.add time_min_each m t
        | Some t' -> if t' > t then Hashtbl.replace time_min_each m t
        )
    | _ -> ()
  ) move_times;
  
  (* on peut en faire des choses avec un cube en 5 secondes *)
  let cost (t, _, alg, _, _, _) =
    t +. (List.fold_left (fun acc m ->
      match Hashtbl.find_opt time_min_each m with
      | Some min_t -> acc +. min_t
      | None -> acc (* WOOPS, it seems we don't know how to do that ! Let's assume it can be done fast *)
    ) 0.0 alg)
  in
  let frontier = ref (
    cross [F] [F; U; D; Bu; Bd; M] @ cross [F; U; D; Bu; Bd; M] [F]
    |> List.map (fun x -> (0.0, x, alg, 0.0, []))
    |> cross_f (fun h (a, b, c, d, e) -> (a, b, c, d, e, h)) [R; L]
    |> List.fold_left (fun acc x -> PrioQueue.insert acc (cost x) x) PrioQueue.empty
    )
  and shortest = ref None in

  let show_time t hist =
    print_float t;
    print_string "       \r"; flush stdout
  in
  let show_best t =
    print_string "new best: "; print_float t; print_newline ()
  in
  let is_greater_than_shortest (t: time): bool =
    match !shortest with
    | None -> false
    | Some (ts, _, _, _, _, _) -> t >= ts
  in
  let start_date = if timeout > 0.0 then Sys.time () else 0.0 in
  while (!frontier <> Empty && !shortest = None) && (timeout = 0.0 || Sys.time () -. start_date < timeout) do
    let _, (time, hand_pos, alg, time0, hist, hand), nfrontier = PrioQueue.extract !frontier in
    frontier := nfrontier;
    show_time time hist;
    move_times
    |> List.filter_map (fun (h1, m, h2, t, t0) ->
      if h1 <> hand_pos || is_greater_than_shortest (t +. time) then None
      else
        let (hl1, hr1), (hl2, hr2) = h1, h2 in
        let nt = t +. if (
          match hand with
          | L -> hl1 <> hl2
          | R -> hr1 <> hr2
        ) then 0.1 else 0.0 in
        Option.map (fun x -> (h1, m, h2, nt, t0, x)) (eat_opt alg m)
    )
    |> List.iter (fun (_, m, h2, t, t0, alg) ->
      let me = (t +. time, h2, alg, t0 +. time0, (hand_pos, m, h2)::hist, hand) in
      if alg = id_alg then (
        if not (is_greater_than_shortest (t +. time)) then (
          show_best (t +. time);
          shortest := Some me
        )
      ) else
        frontier := PrioQueue.insert !frontier (cost me) me
    )
  done;
  match !shortest with
  | None -> None
  | Some (_, _, _, t0, hist, _) -> Some (t0, List.rev hist)

let fastest_maneuver ?(timeout = 0.0) (move_times: (handed_move, float) Hashtbl.t) (alg: alg): (time * maneuver) =
  match fastest_maneuver_opt ~timeout move_times alg with
  | None ->
    if timeout = 0.0 then failwith "that should not happen ... (surely move_time.csv is missing smth)"
    else failwith "ran out of time"
  | Some x -> x
