open Permutation
open Algorithm

type cube = permutation (* of 54 elements *)

let get_id_cube (): cube =
  id_perm 54

let clone_cube (c: cube): cube =
  Array.copy c

let apply_alg_ip (cube: cube) (alg: alg): unit =
  let apply_perm (perm: int array) (cube: int array): unit =
    let taille = Array.length perm in
      let courant =  cube.(perm.(0)) in
      Array.iteri (fun ind x -> if ind <> (taille-1) then cube.(x) <- cube.(perm.(ind + 1)) else cube.(x) <- courant) perm
  in
  let m_apply_move (a: int array list) (cube: int array): unit = 
    List.iter (fun x -> apply_perm x cube) a
  in
  let rec s_of_c (cube: int array) (s: single_move) : unit = 
    match s with
    |'R' -> m_apply_move [[|15; 8; 35; 53|]; [|50; 27; 5; 23|]; [|36; 38; 14; 12|]; [|24; 37; 26; 13|]; [|11; 47; 39; 2|]] cube
    |'L' -> m_apply_move [[|17; 51; 33; 6|]; [|48; 21; 3; 29|]; [|44; 20; 18; 42|]; [|43; 32; 19; 30|]; [|9; 0; 41; 45|]] cube
    |'U' -> m_apply_move [[|18; 9; 12; 15|]; [|20; 11; 14; 17|]; [|0; 6; 8; 2|]; [|3; 7; 5; 1|]; [|19; 10; 13; 16|]] cube
    |'D' -> m_apply_move [[|42; 39; 36; 33|]; [|44; 41; 38; 35|]; [|51; 53; 47; 45|]; [|48; 52; 50; 46|]; [|43; 40; 37; 34|]] cube
    |'F' -> m_apply_move [[|44; 47; 12; 6|]; [|20; 45; 36; 8|]; [|33; 35; 11; 9|]; [|21; 34; 23; 10|]; [|32; 46; 24; 7|]] cube
    |'B' -> m_apply_move [[|53; 42; 0; 14|]; [|51; 18; 2; 38|]; [|39; 41; 17; 15|]; [|27; 40; 29; 16|]; [|26; 52; 30; 1|]] cube
		|'M' -> m_apply_move [[|1; 40; 46; 10|]; [|52; 34; 7; 16|]; [|49; 22; 4; 28|]] cube
		|'E' -> m_apply_move [[|24; 21; 30; 27|]; [|26; 23; 32; 29|]; [|25; 22; 31; 28|]] cube
		|'S' -> m_apply_move [[|43; 50; 13; 3|]; [|19; 48; 37; 5|]; [|31; 49; 25; 4|]] cube
		|'x' -> apply_alg cube [('R', 3); ('L', 1); ('M', 1)]
		|'y' -> apply_alg cube [('U', 3); ('D', 1); ('E', 1)]
		|'z' -> apply_alg cube [('B', 3); ('F', 1); ('S', 1)]
		|'r' -> apply_alg cube [('R', 1); ('M', 3)]
		|'l' -> apply_alg cube [('L', 1); ('M', 1)]
		|'u' -> apply_alg cube [('U', 1); ('E', 3)]
		|'d' -> apply_alg cube [('D', 1); ('E', 1)]
		|'b' -> apply_alg cube [('B', 1); ('S', 3)]
		|'f' -> apply_alg cube [('F', 1); ('S', 1)]
    | _ -> invalid_arg "alg"
  and apply_alg (cube: int array) (alg: alg):unit =
    let rec f (alg: alg): unit =
      match alg with
      | [] -> ()
      | (s, n)::alg -> let rec do_ n = if n <> 0 then (s_of_c cube s; do_ (n-1)) in do_ n; f alg
    in f alg
  in
  apply_alg cube alg

let apply_alg (cube: cube) (alg: alg): cube =
  let cube = clone_cube cube in
  apply_alg_ip cube alg;
  cube

let cube_from_alg (alg: alg): cube =
  let cube = get_id_cube () in
  apply_alg_ip cube alg;
  cube

let inverse_cube (cube: cube): cube =
  inverse_perm cube

let multiply_cubes (cube1: cube) (cube2: cube): cube =
  cube1 $ cube2

(* ROTATION OF THE CUBE *)

(** the map of all the rotations and their associated cube *)
let rotations_tbl, rotations_lst =
  let assoc = 
    ["" ; "z"  ; "z2"  ; "z'"  ; (* front face *)
    "x" ; "xz" ; "xz2" ; "xz'" ; (* up face *)
    "x2"; "x2z"; "x2z2"; "x2z'"; (* back face *)
    "x'"; "x'z"; "x'z2"; "x'z'"; (* down face *)
    "y" ; "yz" ; "yz2" ; "yz'" ; (* left face *)
    "y'"; "y'z"; "y'z2"; "y'z'"] (* right face *)
    |> List.map parse_alg
    |> List.map (fun alg -> (elementalize alg (id_rot ()) |> snd, (cube_from_alg alg, cube_from_alg alg |> inverse_cube)))
  in
  Hashtbl.of_seq (List.to_seq assoc), assoc

let rotate_cube (cube: cube) (rot: cube_rot): cube =
  multiply_cubes (
    Hashtbl.find rotations_tbl rot |> fst
  ) cube

let get_cube_rot (cube: cube): cube_rot =
  let rec aux (s: (cube_rot * (cube * cube)) list) =
    match s with
    | (rot, (c, _))::s ->
      if c.(4) = cube.(4) && c.(22) = cube.(22) then rot
      else aux s
    | _ -> failwith "not a cube"
  in
  aux rotations_lst

let realign (cube: cube): cube * cube_rot =
  let rec aux (s: (cube_rot * (cube * cube)) list) =
    match s with
    | (rot, (c, ic))::s ->
      if c.(4) = cube.(4) && c.(22) = cube.(22) then ic $ cube, rot
      else aux s
    | _ -> failwith "not a cube"
  in
  aux rotations_lst

  (* e_alg functions *)
  
let apply_e_alg_ip (cube: cube) (alg: e_alg): unit =
  let inv_rot = get_cube_rot cube |> inverse_perm in
  apply_alg_ip cube (List.map (fun (m, n) -> char_of_e_move (rotate_e_move m inv_rot), n) alg)

let apply_e_alg (cube: cube) (alg: e_alg): cube =
  let cube = clone_cube cube in
  apply_e_alg_ip cube alg;
  cube

let cube_from_e_alg (alg: e_alg): cube =
  let cube = get_id_cube () in
  apply_e_alg_ip cube alg;
  cube
  
  (* UTIL FUNCTIONS *)

let cube_to_str (cube: cube): string =
  let color (stick: int): char =
    let l = (stick / 3) in
    if l < 3 then 'U'
    else if l > 14 then 'D'
    else if l = 3 || l = 7 || l = 11 then 'F'
    else if l = 4 ||l = 8 || l = 12 then 'R'
    else if  l = 5 || l = 9 || l = 13 then 'B'
    else 'L'
  in
  let cube_perm =
    cube $ [|0; 1; 2; 3; 4; 5; 6; 7; 8; (* UP *)
    12; 13; 14; 24; 25; 26; 36; 37; 38; (* RIGHT *)
    9; 10; 11; 21; 22; 23; 33; 34; 35; (* FRONT *)
    45; 46; 47; 48; 49; 50; 51; 52; 53; (* DOWN *)
    18; 19; 20; 30; 31; 32; 42; 43; 44; (* LEFT *)
    15; 16; 17; 27; 28; 29; 39; 40; 41 (* BACK *)
    |]
  in
  String.init 54 (fun i -> color cube_perm.(i))

let is_solved_cube (c: cube): bool =
  let rec aux i =
    if i = Array.length c then true
    else c.(i) = i && aux (i+1)
  in
  aux 0

module CubeSet = Set.Make(struct type t = cube let compare = compare end)

let get_min_subgroup (generator: CubeSet.t): CubeSet.t =
  let rec aux (frontier: CubeSet.t) (explored: CubeSet.t): CubeSet.t =
    let next, explored = CubeSet.fold (fun c acc ->
        CubeSet.fold (fun c' (next, explored) ->
          let nc = multiply_cubes c c' in
          if CubeSet.mem nc explored then next, explored
          else CubeSet.add nc next, CubeSet.add nc explored
        ) generator acc
      ) frontier (CubeSet.empty, explored)
    in
    if CubeSet.is_empty next then explored
    else aux next explored
  in
  aux (CubeSet.singleton (get_id_cube ())) (CubeSet.singleton (get_id_cube ()))

(* sous groupe des PLL: groupe des permutations des coins et arêtes de la face supérieure *)
let pll_subgroup =
  let f (s: string): cube =
    parse_alg s |> cube_from_alg
  in
  get_min_subgroup CubeSet.(singleton (f "U") |> add (f "R2B2RFR'B2RF'R") |> add (f "R2URUR'U'R'U'R'UR'"))
  (* get_min_subgroup CubeSet.(singleton (f "U") |> add (f "R2B2RFR'B2RF'R") |> add (f "R2URUR'U'R'U'R'UR'") |> add (f "M'UM'UM'U2MUMUMU2") |> add (f "RU2R'U'RU'R'L'U2LUL'UL")) *)

let _ = print_int (CubeSet.cardinal pll_subgroup); print_newline ()

(** the array [to_fill] must be of length 11 *)
let collapse_cube (cube: cube) (to_fill: int array): unit =
  for i = 0 to 10 do
    to_fill.(i) <- cube.(i * 5 + 0)
               + 54 * (cube.(i * 5 + 1)
                + 54 * (cube.(i * 5 + 2)
                 + 54 * cube.(i * 5 + 3)
                 )
                );
    if i <> 10 then
      to_fill.(i) <- to_fill.(i) + cube.(i * 5 + 4) * (54 * 54 * 54 * 54);
  done

let create_collapsed (cube: cube): int array =
  let to_fill = Array.make 11 0 in
  collapse_cube cube to_fill;
  to_fill

let expand_cube (array: int array) (to_fill: cube): unit =
  for i = 0 to 10 do
    let n = array.(i) in
    to_fill.(i * 5 + 0) <- n mod 54;
    let n = n / 54 in
    to_fill.(i * 5 + 1) <- n mod 54;
    let n = n / 54 in
    to_fill.(i * 5 + 2) <- n mod 54;
    let n = n / 54 in
    to_fill.(i * 5 + 3) <- n mod 54;
    if i <> 10 then
      let n = n / 54 in
      to_fill.(i * 5 + 4) <- n mod 54;
  done

let pp_cube (cube: cube): string =
  let color (stick: int): char =
    let l = (stick / 3) in
    if l < 3 then 'U'
    else if l > 14 then 'D'
    else if l = 3 || l = 7 || l = 11 then 'F'
    else if l = 4 ||l = 8 || l = 12 then 'R'
    else if  l = 5 || l = 9 || l = 13 then 'B'
    else 'L'
  in
  List.fold_left (fun acc i -> acc ^ 
    match i with
    | -1 -> "   "
    | -2 -> "\n"
    | i -> " " ^ String.make 1 (color cube.(i)) ^ " ") ""
  [-1; -1; -1; 0; 1; 2; -2; -1; -1; -1; 3; 4; 5; -2; -1; -1; -1; 6; 7; 8; -2;
  9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20; -2;
  21; 22; 23; 24; 25; 26; 27; 28; 29; 30; 31; 32; -2;
  33; 34; 35; 36; 37; 38; 39; 40; 41; 42; 43; 44; -2;
  -1; -1; -1; 45; 46; 47; -2; -1; -1; -1; 48; 49; 50; -2; -1; -1; -1; 51; 52; 53
  ]
