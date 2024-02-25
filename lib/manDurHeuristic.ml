open Algorithm
open CuberProfile
open Permutation


let _TIME_PRECISION = 100.

let rec pow a = function
| 0 -> 1
| 1 -> a
| n -> 
  let b = pow a (n / 2) in
  b * b * (if n mod 2 = 0 then 1 else a)

let ordinal_hpos (m: single_hand_pos): int =
  match m with
  | F -> 0
  | U -> 1
  | D -> 2
  | Bu -> 3
  | Bd -> 4
  | M -> 5
  | Af -> 6
  | Out -> 7

let hpos_from_ordinal (i: int): single_hand_pos =
  match i with
  | 0 -> F
  | 1 -> U
  | 2 -> D
  | 3 -> Bu
  | 4 -> Bd
  | 5 -> M
  | 6 -> Af
  | 7 -> Out
  | _ -> invalid_arg "i (hpos)"

let ordinal_e_move (m: e_move): int =
  match m with
  | U -> 0 | R -> 1 | F -> 2 | D -> 3 | L -> 4 | B -> 5

let e_move_from_ordinal (i: int): e_move =
  match i with
  | 0 -> U | 1 -> R | 2 -> F | 3 -> D | 4 -> L | 5 -> B | _ -> invalid_arg "i (e_move)"

module IntSet = Set.Make(Int)

(** THOSE ALGS ARE REVERTED *)
let output_maneuver_times
    ?(verbose = false)
    (depth: int)
    (file: out_channel)
    (move_times: (handed_move, float) Hashtbl.t)
    : unit =
  let move_times =
    move_times
    |> Hashtbl.to_seq
    |> Seq.filter_map (fun ((from_, alg, to_), t) ->
      if t <= 0.0 then None else
        Some (from_, alg, to_,
        t (*(if from_ = to_ || (match alg with | ('R', _)::_ | ('L', _)::_ -> true | _ -> false) then 0. else 0.05) +. (if (let (x, y) = to_ in x = Out || y = Out) then 0.0 else 0.)*)
        )
      )
    |> List.of_seq
  in

  let min_time = List.fold_left (fun acc (_, alg, _, t) -> if alg = [] then acc else min acc t) 500. move_times in

  let move_times_dict = Hashtbl.create 64 in
  cross [F; U; D; Bu; Bd; M; Af; Out] [F; U; D; Bu; Bd; M; Af; Out]
  |> cross [R; L]
  |> List.iter (fun (hand, hand_pos) ->
    Hashtbl.add move_times_dict (hand, hand_pos)
    (move_times
    |> List.filter_map (fun (h1, m, h2, t) ->
      if h1 <> hand_pos (*|| is_greater_than_shortest (t +. time)*) then None
      else
        let (hl1, hr1), (hl2, hr2) = h1, h2 in
        let nt = t +. (if (
          match hand with
          | L -> hl1 <> hl2
          | R -> hr1 <> hr2
          ) then 0.2 else 0.0
        )
        in
        Some (m, h2, nt, m)
    ))
  );

  let cube_rots = Array.make 24 [||] in
  cube_rots.(0) <- id_rot ();

  let alg_count = (pow 18 (depth + 1) - 1) / 17 in
  let explored = ref IntSet.empty in
  let algs_explored = ref IntSet.empty in

  let ser_alg (alg: e_alg): int =
    List.fold_left (fun acc (m, n) -> (ordinal_e_move m) * 3 + n + acc * 18) 0 alg
  in
  let deser_alg (ser: int): e_alg =
    let rec f (ser: int) (acc: e_alg): e_alg =
      if ser = 0 then acc
      else
        let s = (ser - 1) mod 18 in
        f (ser / 18) ((e_move_from_ordinal (s / 3), s mod 3)::acc)
    in f ser []
  in
  let ser_pos ((h1, h2): hand_pos) (alg: e_alg): int =
    ser_alg alg lsl 6 lor (ordinal_hpos h1 lsl 3) lor ordinal_hpos h2
  in
  let deser_pos (ser: int): hand_pos * e_alg =
    let hp = (hpos_from_ordinal ((ser land 0b111000) lsr 3), hpos_from_ordinal (ser land 0b111)) in
    hp, deser_alg (ser lsr 6)
  in
  let log_18 = log 18. in
  let cost (t, alg_ser, _, _) =
    t -. min_time *. ceil (log (alg_ser lsr 6 |> float_of_int) /. log_18)
  in

  let frontier = ref (
    (cross [F; U; D; Bu; Bd; M] [F; U; D; Bu; Bd; M])
    |> List.map (fun x -> (0.0, ser_pos x [], 0))
    |> cross_f (fun ha (t, ser, cr) -> (t, ser, cr, ha)) [R; L]
    |> List.fold_left (fun acc x -> PrioQueue.insert acc (cost x) x) PrioQueue.empty
  ) in

  (** size of algs_explored *)
  let i = ref 0 in
  let last_print_i = ref 0. in
  let explored_time = ref 0. in

  while !frontier <> PrioQueue.empty && (!i * 100 / alg_count < 94) do
    let _, nearest, nfrontier = PrioQueue.extract !frontier in
    frontier := nfrontier;
    let (time, ser, cube_rot_i, hand) = nearest in
    explored_time := time;
    let cube_rot = cube_rots.(cube_rot_i) in
    let hand_pos, alg = deser_pos ser in
    if not (List.length alg > depth || IntSet.mem ser !explored) then
      begin
        Hashtbl.find move_times_dict (hand, hand_pos)
        |> List.iter (fun (m, h2, t, e_m) ->
          let e_m, cube_rot = elementalize m cube_rot in
          let newalg = e_compose_simplified e_m alg in
          let cube_rot_i =
            match Array.find_index ((=) cube_rot) cube_rots with
            | Some i -> i
            | None ->
              (let i = Array.find_index ((=) [||]) cube_rots |> Option.get in
              cube_rots.(i) <- cube_rot;
              i)
          in
          let me = (t +. time, ser_pos h2 newalg, cube_rot_i, hand) in
          frontier := PrioQueue.insert !frontier (cost me) me
        );
        explored := IntSet.add ser !explored;
        let alg_ser = ser lsr 6 in
        if not (IntSet.mem alg_ser !algs_explored) then
          begin
            incr i;
            if Sys.time () -. !last_print_i > 0.5 && verbose then
              begin
                print_string (string_of_int !i ^ " / " ^ string_of_int alg_count);
                print_string "   ";
                print_string (string_of_float !explored_time);
                print_string "       \r";
                flush stdout;
                last_print_i := Sys.time ()
              end;
            algs_explored := IntSet.add alg_ser !algs_explored;
            output_byte file (List.length alg);
            List.iter (fun (m, n) -> output_byte file (ordinal_e_move m * 3 + n - 1)) alg;
            output_binary_int file (time *. _TIME_PRECISION |> int_of_float)
          end
      end
  done;
  output_binary_int file (!explored_time *. _TIME_PRECISION |> int_of_float);
  seek_out file 0;
  output_binary_int file !i

let create_table ?(verbose = true) ?(depth = 5) (name: string) (prof: profile): unit =
  let move_times = load_move_times prof in
  let filename = "maneuver_dur_table_" ^ name ^ ".dat" in
  let file = open_out filename in
  output_maneuver_times ~verbose depth file move_times;
  close_out file;
  print_endline "\ndone !"

let load_table (name: string): (e_alg, float) Hashtbl.t * float =
  let tbl = Hashtbl.create 100 in
  let file = open_in ("maneuver_dur_table_" ^ name ^ ".dat") in

  let input_time () = ((input_binary_int file |> float_of_int) /. _TIME_PRECISION) in
  let size = input_binary_int file in
  print_endline ("alg count : " ^ string_of_int size);
  for i = 0 to size-1 do
    let alg_l = input_byte file in
    print_endline ("alg " ^ string_of_int i ^ ": size " ^ string_of_int alg_l);
    let alg = ref [] in
    for i = 0 to alg_l-1 do
      let b = input_byte file in
      print_int b; print_string " ";
      alg := (e_move_from_ordinal (b/3), (b mod 3) + 1)::!alg
    done;
    print_newline ();
    print_endline (e_alg_to_string !alg);
    (* we collect the algorithms in reverse order, but they were saved reversed too *)
    Hashtbl.add tbl !alg (input_time ())
  done;
  let max_time = input_time () in
  close_in file;
  tbl, max_time


let test_create depth =
  create_table "default" ~depth (load_profile "default" |> Option.get)

let test_load () =
  load_table "default"
