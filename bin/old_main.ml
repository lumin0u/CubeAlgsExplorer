open Rubiks_cube
open CuberProfile
open AlgEvaluator
open Algorithm
open PatateFinder
open Cubes

let move_times = load_move_times default_profile
let fastest (a: string) = a |> parse_alg |> fastest_chaining move_times
let j_perm_moves = fastest "RUR'F'RUR'U'R'FR2U'R'"
let f_perm_moves = fastest "R' U' F' R U R' U' R' F R2 U' R' U' R U R' U R"
let o_perm_moves = fastest "FRU'R'U'RUR'F'RUR'U'R'FRF'"
let h_perm_moves = fastest "M2UM2U2M2UM2"
let cube_oll_moves = fastest "rU2R'U'RU'r'"

let profile1 =
  {ease_right_U2 = 1.6; ease_left_U2 = 1.3; hand_M = L; ease_M' = 1.; ease_M = 1.5; ease_M2 = 1.2}
let profile2 =
  {ease_right_U2 = 1.2; ease_left_U2 = 1.3; hand_M = L; ease_M' = 1.; ease_M = 1.7; ease_M2 = 1.2}


let pp_hand_pos (shp: single_hand_pos): string =
  match shp with | M -> "M" | F -> "F" | U -> "U" | D -> "D" | Bu -> "B" | Bd -> "P" | Af -> "f" | Out -> "O"

let pp_list (pp_a: 'a -> string) ?(s = "[") ?(e = "]") ?(sep = "; ") (l: 'a list): string =
  match l with
  | [] -> s ^ e
  | x::l -> List.fold_left (fun acc x -> acc ^ sep ^ (pp_a x)) (s ^ (pp_a x)) l ^ e
(*
let pp_array (pp_a: 'a -> string) (a: 'a array): string =
  let n = Array.length a in
  match a with
  | [||] -> "[||]jperm_moves"
  | a -> fst (Array.fold_left (fun (acc, i) x -> acc ^ (pp_a x) ^ (if i = n-1 then "" else "; "), i+1) ("[|", 0) a) ^ "|]"
*)
let () = 
  let (t, _, l) = h_perm_moves in
  print_string (pp_list (fun ((shp, shp'), alg, (shp'', shp''')) ->
    let alg_s = alg_to_string alg in
    let alg_s = alg_s ^ (String.make (4 - String.length alg_s) ' ') in
    (pp_hand_pos shp) ^ ";" ^ (pp_hand_pos shp') ^ " -> " ^ alg_s ^ " -> " ^ (pp_hand_pos shp'') ^ ";" ^ (pp_hand_pos shp''')
  ) l ~s:"" ~e:"" ~sep:"\n");
  print_string ", ";
  print_float t;
  print_newline ()

let test_patate () =
  print_string (e_alg_to_string (parse_alg "RuRyR" |> elementalize_no_rot));
  print_newline ();
  let f (s: string): e_alg =
    parse_alg s |> elementalize_no_rot
  in prev_patate (SC.singleton Cube.solved_state) [f "U"; f "R2B2RFR'B2RF'R"; f "R2URUR'U'R'U'R'UR'"] 150 |> ignore
  ;prev_patate (SC.singleton Cube.solved_state) [f "U"; f "RU2R'U'RU'R'"; f "FRUR'U'F'"] 70000 |> ignore

