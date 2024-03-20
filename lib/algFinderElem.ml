open Algorithm
open CuberProfile
open Cube


(* time is float, locally *)
open struct type time = float end

(** Si [l = q @ l'] élémentairement, [eat_opt l q] s'évalue en [Some l'].
 S'évalue en [None] sinon *)
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
  | [] -> true
  | _ -> false


let hand_pos_to_string ((h1, h2): hand_pos): string =
  let str_hp (hp: single_hand_pos): string = 
    match hp with
    | Af -> "Af" | F -> "F" | U -> "U" | Bu -> "B" | Bd -> "P" | D -> "D" | Out -> "O" | M -> "M"
  in
  str_hp h1 ^ ":" ^ str_hp h2

let maneuver_to_string maneuver =
  List.fold_left (fun acc (h1, a, h2) ->
    (if acc = "" then ("[" ^ hand_pos_to_string h1 ^ "] ") else acc) ^
    (
      match a with 
      | [] -> "[" ^ hand_pos_to_string h2 ^ "]"
      | [_] -> alg_to_string a
      | _ -> "(" ^ alg_to_string a ^ ")"
    ) ^ " "
  ) "" maneuver

(* module StateSet = Set.Make(struct type t = hand_pos * cube let compare = compare end) *)
module StateSet = Set.Make(struct type t = int * int let compare = compare end)
module PermutationSet = Set.Make(struct type t = int let compare = compare end)

(*  multiply_cubes (inverse_cube target) cube  *)
(** Will try to compute [count] maneuvers, may return less if [timeout] is set to a positive value. *)
let fastest_maneuvers
    ?(timeout = 0.0)
    ?(heuristic = 0.1)
    ?(quotient_group = CubeSet.singleton (get_id_cube ()))
    (count: int)
    (move_times: (handed_move, float) Hashtbl.t)
    (cube: cube)
    : (time * maneuver) list =

  let shortests = ref [] in
  let counter = ref 0 in

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
        Some (m, cube_from_alg m, h2, nt +. (if is_regrip m then 0.05 else 0.0), t0)
    ))
  );

  let positions_dist = Hashtbl.create 0 in

  (* setup of the communication with the python server *)
  let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, 8085) in
  let sock = Unix.(socket PF_INET SOCK_STREAM 0) in
  Unix.connect sock addr;
  let in_ch = Unix.in_channel_of_descr sock
  and out_ch = Unix.out_channel_of_descr sock in

  (* ask the python server for an heuristic of the distance to the solved cube *)
  let ask_dist (cube: cube): int =
    let realigned = realign cube |> fst in
    match Hashtbl.find_opt positions_dist realigned with
    | Some d -> d
    | None ->
      output_string out_ch (cube_to_str realigned ^ "\n");
      flush out_ch;
      let res = input_line in_ch |> int_of_string in
      Hashtbl.add positions_dist realigned res;
      res
  in

  (* on peut en faire des choses avec un cube en 5 secondes *)
  let cost (t, _, cube, dist, _, _, _, _) =
    t +. heuristic *. float_of_int dist
  in

  let to_state hand_pos cube hand =
    (Hashtbl.hash (hand_pos, hand, cube), Hashtbl.hash cube)
  in

  let states_tbl = ref StateSet.empty
  and explored_cubes = ref PermutationSet.empty
  in
  let dist = ask_dist cube in
  let frontier =
    let start =(*R'FRUR'F'RFU'F'*)
      cross [F] [F; U; D; Bu; Bd; M] @ cross [F; U; D; Bu; Bd; M] [F]
      |> cross (CubeSet.to_list quotient_group)
      |> cross [R; L]
      |> List.map (fun (h, (c, hp)) -> (0.0, hp, multiply_cubes c cube, dist, 0.0, [], h, []))
    in
    List.iter (fun (_, hp, cube, _, _, _, hand, _) -> states_tbl := StateSet.add (to_state hp cube hand) !states_tbl) start;
    ref (List.fold_left (fun acc x -> PrioQueue.insert acc (cost x) x) PrioQueue.empty start)
  in

  (* ### EXPLORATION ### *)
  let explore move_times_dict frontier explored_cubes =
    let _, (time, hand_pos, cube, dist, time0, hist, hand, used_m), nfrontier = PrioQueue.extract !frontier in
    frontier := nfrontier;
    (* print_endline (string_of_int dist ^ " " ^ maneuver_to_string (List.rev hist) ^ " " ^ string_of_float time); *)
    if is_solved_cube cube then begin
      (* solution found *)
      print_newline ();
      print_endline ("from forward : " ^ maneuver_to_string (List.rev hist));
      incr counter;
      shortests := (time0, hist)::!shortests
    end
    else begin
      (* explore neighbours *)
      explored_cubes := PermutationSet.add (Hashtbl.hash cube) !explored_cubes;
      Hashtbl.find move_times_dict (hand, hand_pos)
      |> List.iter (fun (m, tocube, h2, t, t0) ->
        (* tocube is the result of apply_alg id_cube m *)
        let obv_bad =
          match hist with
          | (_, last_move, _)::_ -> is_regrip m && is_regrip last_move
          | _ -> is_regrip m
        in
        if not obv_bad then
          let new_moves = List.filter_map (fun (m, _) -> if List.mem m used_m then None else Some m) m in
          let nt = t +. if new_moves = [] then 0.0 else 0.5 *. heuristic
          in
          let (cube', dist') =
            if m = [] then cube, dist
            else let c = multiply_cubes cube tocube in c, ask_dist c in
          
          (* this is kind of cheating but it works *)
          let nt = if PermutationSet.mem (Hashtbl.hash cube') !explored_cubes then nt +. 0.08 else nt in

          let state = to_state h2 cube' hand in
          if not (StateSet.mem state !states_tbl) then
            begin
              states_tbl := StateSet.add state !states_tbl;
              let me = (nt +. time, h2, cube', dist', t0 +. time0, (hand_pos, m, h2)::hist, hand, new_moves @ used_m) in
              frontier := PrioQueue.insert !frontier (cost me) me
            end
      )
    end
  in

  let nodes = ref 0 in
  let start_date = Sys.time () in

  (* ### 'MAIN LOOP' ### *)
  while !counter < count && (timeout = 0.0 || Sys.time () -. start_date < timeout) do
    incr nodes;
    if !nodes mod 17 = 0 then
      (print_int !nodes; print_string "  \r"; flush stdout);
    explore move_times_dict frontier explored_cubes
  done;
  print_newline ();
  print_float (Sys.time () -. start_date); print_newline ();
  List.rev_map (fun (t0, hist) -> (t0, List.rev hist)) !shortests

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
    ?(heuristic = 0.1)
    (move_times: (handed_move, float) Hashtbl.t)
    (cube: cube)
    : time * maneuver =
  match fastest_maneuvers ~heuristic 1 move_times cube with
  | [] -> failwith "erreur erreur erreur (AlgFinderElem.fastest_maneuver)"
  | x::_ -> x
