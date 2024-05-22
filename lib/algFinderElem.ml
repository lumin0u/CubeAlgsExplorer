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

let pp_array array =
  Array.fold_left (fun acc x -> acc ^ string_of_int x ^ "; ") "[|" array ^ "|]"

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

let alg_of_maneuver (m: maneuver): alg =
  List.fold_left (fun acc (_, a, _) -> compose_alg acc a) id_alg m

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
    ((in_ch, out_ch): in_channel * out_channel)
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
    |> List.filter_map (fun (h1, m, h2, t, _) ->
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
        Some (m, cube_from_alg m, h2, nt +. (if m = [] then 0.0 else 0.0))
    ))
  );

  let positions_dist = Hashtbl.create 0 in

  (* ask the python server for an heuristic of the distance to the solved cube *)
  let ask_dist (cube: cube): int =
    let realigned, _ = realign cube in
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
  let cost (t, _, _, dist, _, _, n) =
    t +. heuristic *. (float_of_int dist -. float_of_int n *. 0.3)
  in

  let to_state hand_pos cube hand =
    (Hashtbl.hash (hand_pos, hand, cube), Hashtbl.hash cube)
  in

  let states_tbl = Hashtbl.create 100
  (* and explored_cubes = ref PermutationSet.empty *)
  in
  let frontier = Heap.create () in
  let start =(*R'FRUR'F'RFU'F'*)
    cross [F] [F; U; D; Bu; Bd; M] @ cross [F; U; D; Bu; Bd; M] [F]
    |> cross (CubeSet.to_list quotient_group)
    |> cross [R; L]
    |> List.map (fun (h, (c, hp)) ->
      let cube = multiply_cubes c cube in
      let dist = ask_dist cube in
      (0.0, hp, create_collapsed cube, dist, h, [], 0))
  in
  List.iter (fun (_, hp, cube, _, hand, _, _) -> Hashtbl.add states_tbl (to_state hp cube hand) None) start;
  List.iter (fun x -> Heap.add (cost x) x frontier) start;

  let find_hist state hp_end =
    let rec aux state acc =
      match Hashtbl.find states_tbl state with
      | Some((hp, m), super) -> aux super ((hp, m)::acc)
      | None -> acc
    in
    let rec aux1 man acc =
      match man with
      | (h1, m1)::(h2, m2)::man -> aux1 ((h2, m2)::man) ((h1, m1, h2)::acc)
      | [(h, m)] -> (h, m, hp_end)::acc
      | [] -> []
    in
    aux1 (aux state []) []
  in

  let common_cube = Array.make 54 0 in
  (* ### EXPLORATION ### *)
  let explore () =
    let _, (time, hand_pos, ccube, dist, hand, used_m, n) = Heap.take_min frontier in
    let super_state_hash = to_state hand_pos ccube hand in
    let cube = common_cube in
    expand_cube ccube cube;
    (* print_endline (string_of_int dist ^ " " ^ maneuver_to_string (List.rev hist) ^ " " ^ string_of_float time); *)
    if is_solved_cube cube then begin
      let hist = find_hist super_state_hash hand_pos in
      let me_to_alg = alg_of_maneuver hist in
      if List.for_all (fun (_, _, a) -> me_to_alg <> a) !shortests then begin
        (* we do not want maneuvers that are too similar *)
        (* solution found *)
        print_newline ();
        print_endline ("from forward : " ^ maneuver_to_string (List.rev hist));
        incr counter;
        shortests := (time, hist, me_to_alg)::!shortests
      end
    end
    else begin
      (* explore neighbours *)
      let super_state = Hashtbl.find states_tbl super_state_hash in
      Hashtbl.find move_times_dict (hand, hand_pos)
      (* compute time now ... *)
      |> List.filter_map (fun (m, tocube, h2, t) ->
        (* tocube is the result of apply_alg id_cube m *)
        let obv_bad =
          match super_state with
          | Some((_, []), _) | None -> m = []
          | Some((_, [(m', _)]), _) ->
            begin
              match m with
              | [(m, _)] -> m = m'
              | _ -> false
            end
          | _ -> false
        in
        if obv_bad then
          None
        else
          let new_moves = List.filter_map (fun (m, _) -> if List.mem m used_m then None else Some m) m in
          let t = t +. if new_moves = [] then 0.0 else 0.5 *. heuristic in
          Some (m, tocube, h2, t, new_moves)
      )
      (* ... so that it can be sorted and pushed in the queue in the right order *)
      |> List.sort (fun (_, _, _, t1, _) (_, _, _, t2, _) -> Float.compare t1 t2)
      |> List.iter (fun (m, tocube, h2, t, new_moves) ->
        (* tocube is the result of apply_alg id_cube m *)
        let cube_changed = m <> [] in
        let cube' =
          if cube_changed then multiply_cubes cube tocube
          else cube
        in

        let collapsed = create_collapsed cube' in
        let state = to_state h2 collapsed hand in
        if not (Hashtbl.mem states_tbl state) then
          begin
            let dist' = if cube_changed then ask_dist cube' else dist in
            (* this is kind of cheating but it works *)
            (* if PermutationSet.mem (Hashtbl.hash cube') !explored_cubes then 0.05 else *)
            let cost_add = 0.0 in

            Hashtbl.add states_tbl state (Some((hand_pos, m), super_state_hash));
            let me = (t +. time, h2, collapsed, dist', hand, new_moves @ used_m, n + List.length m) in
            Heap.add (cost me +. cost_add) me frontier
          end
      )
    end
  in

  let nodes = ref 0 in
  let start_date = Sys.time () in

  (* ### 'MAIN LOOP' ### *)
  while !counter < count && (timeout = 0.0 || Sys.time () -. start_date < timeout) do
    incr nodes;
    (if !nodes mod 31 = 0 then
      (print_int !nodes; print_string "    "; print_int (Hashtbl.length states_tbl); print_string "    "; print_int (Heap.length frontier); print_string "  \r"; flush stdout));
    (* if Heap.length frontier > 1000000 then
      (print_endline "\nCUT"; Heap.cut_half frontier); *)
    explore ()
  done;
  print_newline ();
  print_float (Sys.time () -. start_date); print_newline ();
  List.rev_map (fun (t0, hist, _) -> (t0, List.rev hist)) !shortests

let fastest_maneuver_opt
    ?(timeout = 0.0)
    ?(heuristic = 0.1)
    (move_times: (handed_move, float) Hashtbl.t)
    (cube: cube)
    (ch: in_channel * out_channel)
    : (time * maneuver) option =
  match fastest_maneuvers ~timeout ~heuristic 1 move_times cube ch with
  | [] -> None
  | x::_ -> Some x


let fastest_maneuver
    ?(heuristic = 0.1)
    (move_times: (handed_move, float) Hashtbl.t)
    (cube: cube)
    (ch: in_channel * out_channel)
    : time * maneuver =
  match fastest_maneuvers ~heuristic 1 move_times cube ch with
  | [] -> failwith "erreur erreur erreur (AlgFinderElem.fastest_maneuver)"
  | x::_ -> x
