open Algorithm

type hand = L | R

type single_hand_pos = F | U | D | Bu | Bd | M | Af | Out
type hand_pos = single_hand_pos * single_hand_pos
type handed_move = hand_pos * alg * hand_pos
type maneuver = (hand_pos * alg * hand_pos) list

let cross (l1: 'a list) (l2: 'b list): ('a * 'b) list =
  List.fold_left (fun acc x -> (List.map (fun y -> (x, y)) l2) @ acc) [] l1

let cross_f (f: 'a -> 'b -> 'c) (l1: 'a list) (l2: 'b list): 'c list =
  List.fold_left (fun acc x -> (List.map (f x) l2) @ acc) [] l1


type profile = {
  ease_right_U2: float;
  ease_left_U2: float;
  hand_M: hand;
  ease_M': float;
  ease_M: float;
  ease_M2: float;
  }

let default_profile = {
  ease_right_U2 = 1.4;
  ease_left_U2 = 1.4;
  hand_M = L;
  ease_M' = 1.1;
  ease_M = 1.4;
  ease_M2 = 1.2}

(* float of string and recognize comma as a point *)
let csv_parse_float (s: string): float =
  float_of_string (String.map (fun c -> if c = ',' then '.' else c) s)

let is_valid_profile_name (name: string): bool =
  not (String.contains name '\n') && name <> "name" && name <> "default" && Str.string_match (Str.regexp {|^[a-z_0-9]+$|}) name 0


let load_profiles ?(file_name = "profiles.csv") ((): unit): (string * profile) list =
  if not (Sys.file_exists file_name) then []
  else
  let profiles = Csv.load file_name in
  ("default", default_profile)::List.filter_map (function
    | s::v1::v2::v3::v4::v5::v6::_ when is_valid_profile_name s -> Some (s, {
      ease_right_U2 = csv_parse_float v1;
      ease_left_U2 = csv_parse_float v2;
      hand_M = (match v3 with | "R" -> R | "L" -> L | _ -> invalid_arg "unknown hand (must be R or L)");
      ease_M' = csv_parse_float v4;
      ease_M = csv_parse_float v5;
      ease_M2 = csv_parse_float v6;
      })
    | _ -> None
    ) profiles

(*
  tente de charger le profile 'name'. Si le profile n'existe
  pas, aucun n'est créé et s'évalue en 'default_profile'
*)
let load_profile ?(file_name = "profiles.csv") (name: string): profile option =
  List.assoc_opt name (load_profiles ~file_name ())


let save_profile ?(file_name = "profiles.csv") (name: string) (prof: profile): unit =
  if not (is_valid_profile_name name) then
    invalid_arg "profile name"
  else
  let profiles = Csv.load file_name in
  let entry =
    name
    :: string_of_float prof.ease_right_U2
    :: string_of_float prof.ease_left_U2
    :: (match prof.hand_M with | R -> "R" | L -> "L")
    :: string_of_float prof.ease_M'
    :: string_of_float prof.ease_M
    :: string_of_float prof.ease_M2
    :: []
  in
  let rec replace (l: string list list): string list list =
    match l with
    | [] -> [entry]
    | (s::_)::l when s = name -> entry::l
    | x::l -> x::replace l
  in
  Csv.save file_name (replace profiles)

let delete_profile ?(file_name = "profiles.csv") (name: string): bool =
  if not (Sys.file_exists file_name) then false
  else
  let profiles = Csv.load file_name in
  let rec remove (l: string list list) (acc: string list list): string list list * bool =
    match l with
    | [] -> List.rev acc, false
    | (s::_)::l when s = name -> List.rev_append acc l, true
    | x::l -> remove l (x::acc)
  in
  let profiles, removed = remove profiles [] in
  Csv.save file_name profiles;
  removed

(* special values:
  O is Out
  a is Any
  b is any that can Be different than a
  h is Holding (any that is not Out)
  f (Af) is after F'
  B is B from up
  P is B from down
  *)
let parse_single_hand_pos (c: char): single_hand_pos = 
  match c with
  | 'F' -> F
  | 'U' -> U
  | 'D' -> D
  | 'B' -> Bu
  | 'P' -> Bd
  | 'M' -> M
  | 'f' -> Af
  | 'O' -> Out
  | x -> failwith ("nope what is " ^ (String.make 1 x))

let parse_hand_positions (from: string) (to_: string): (hand_pos * hand_pos) list =
  let aux (f: char) (t: char): (single_hand_pos * single_hand_pos) list =
    match f with
    | 'h' -> [(F, F); (U, U); (D, D); (M, M); (Af, F)]
    | 'a' ->
      (match t with
      | 'a' -> [(F, F); (U, U); (D, D); (Bu, Bu); (Bd, Bd); (M, M); (Out, Out)]
      | 'b' -> cross [F; U; D; Bu; Bd; M; Af; Out] [F; U; D; Bu; Bd; M]
      | p -> cross [F; U; D; Bu; Bd; M; Af; Out] [parse_single_hand_pos p]
      )
    | p ->
      (match t with
      | 'a' -> cross [parse_single_hand_pos p] [F; U; D; Bu; Bd; M]
      | p' -> [(parse_single_hand_pos p, parse_single_hand_pos p')]
      )
  in let left_positions = aux from.[0] to_.[0]
  and right_positions = aux from.[1] to_.[1] in
  cross left_positions right_positions
  |> List.map (fun ((a, b), (c, d)) -> ((a, c), (b, d)))

let load_move_times ?(file_name = "move_time.csv") (prof: profile): (handed_move, float) Hashtbl.t =
  let tbl = Hashtbl.create 100 in
  let csv = Csv.load file_name in
  (* special *)
  let left_U' = ref 0.0
  and right_U = ref 0.0
  and left_u' = ref 0.0
  and right_u = ref 0.0
  and default_M' = ref 0.0
  in
  List.iter (
    fun row ->
      match row with
      | _::"#M'"::_::time::[] -> default_M' := csv_parse_float time
      | from_pos::alg::to_pos::time::[] ->
        let alg = String.trim alg in
        if from_pos = "" then () else
        let time =
          if time.[0] = '#' then
            match time with
            | "#left_U2" -> prof.ease_left_U2 *. !left_U'
            | "#left_u2" -> prof.ease_left_U2 *. !left_u'
            | "#right_U2" -> prof.ease_right_U2 *. !right_U
            | "#right_u2" -> prof.ease_right_U2 *. !right_u

            | "#right_M'" -> (if prof.hand_M = R then 1. else 2.) *. prof.ease_M' *. !default_M'
            | "#right_M2" -> (if prof.hand_M = R then 1. else 2.) *. prof.ease_M2 *. prof.ease_M' *. !default_M'
            | "#right_M" -> (if prof.hand_M = R then 1. else 2.) *. prof.ease_M *. prof.ease_M' *. !default_M'

            | "#left_M'" -> (if prof.hand_M = L then 1. else 2.) *. prof.ease_M' *. !default_M'
            | "#left_M2" -> (if prof.hand_M = L then 1. else 2.) *. prof.ease_M2 *. prof.ease_M' *. !default_M'
            | "#left_M" -> (if prof.hand_M = L then 1. else 2.) *. prof.ease_M *. prof.ease_M' *. !default_M'
            | _ -> invalid_arg ("unkown " ^ time ^ " in " ^ file_name)
          else
            csv_parse_float time
        in
        let alg =
          if alg = "#right_U" then
            begin
              right_U := time;
              parse_alg "U"
            end
          else if alg = "#left_U'" then 
            begin
              left_U' := time;
              parse_alg "U'"
            end
          else if alg = "#right_u" then
            begin
              right_u := time;
              parse_alg "u"
            end
          else if alg = "#left_u'" then
            begin
              left_u' := time;
              parse_alg "u'"
            end
          else
            parse_alg alg
        in
        parse_hand_positions from_pos to_pos
        |> List.iter (fun (from_, to_) -> Hashtbl.replace tbl (from_, alg, to_) time)
      | _ -> ()
    ) (List.tl csv); (* ignore the first line of the csv *)
  tbl
