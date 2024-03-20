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

(* e_alg cube functions *)

let xd_cube = cube_from_alg [('x', 1)] (* this cube has been x'd *)
and yd_cube = cube_from_alg [('y', 1)]
and zd_cube = cube_from_alg [('z', 1)]

let rotate_cube (cube: cube) (rot: cube_rot): cube =
  multiply_cubes cube (
    match rot.(0), rot.(1) with
    | 0, 1 -> get_id_cube ()
    | 0, 2 -> yd_cube
    | 0, 4 -> yd_cube $* 2
    | 0, 5 -> yd_cube $* 3
    | 1, 0 -> zd_cube $ yd_cube $* 2
    | 1, 2 -> zd_cube $ yd_cube
    | 1, 3 -> zd_cube
    | 1, 5 -> zd_cube $ yd_cube $* 3
    | 2, 0 -> zd_cube $* 3 $ xd_cube
    | 2, 1 -> xd_cube
    | 2, 3 -> zd_cube $ xd_cube
    | 2, 4 -> zd_cube $* 2 $ xd_cube
    | 3, 1 -> xd_cube $* 2
    | 3, 2 -> yd_cube $ xd_cube $* 2
    | 3, 4 -> zd_cube $* 2
    | 3, 5 -> yd_cube $* 3 $ xd_cube $* 2
    | 4, 0 -> zd_cube $* 3
    | 4, 2 -> yd_cube $ xd_cube
    | 4, 3 -> zd_cube $ xd_cube $* 2
    | 4, 5 -> yd_cube $* 3 $ xd_cube $* 3
    | 5, 0 -> zd_cube $* 3 $ xd_cube $* 3
    | 5, 1 -> xd_cube $* 3
    | 5, 3 -> zd_cube $ xd_cube $* 3
    | 5, 4 -> zd_cube $* 2 $ xd_cube $* 3
    | _ -> invalid_arg "rotate_cube: rot is not a cube_rot"
  )

let get_cube_rot (cube: cube): cube_rot =
  match cube.(4), cube.(25) with
  | 4, 25 -> id_rot ()
  | 4, 22 -> y_rot
  | 4, 31 -> y_rot $* 2
  | 4, 28 -> y_rot $* 3
  | 25, 4 -> z_rot $ y_rot $* 2
  | 25, 22 -> z_rot $ y_rot
  | 25, 49 -> z_rot
  | 25, 28 -> z_rot $ y_rot $* 3
  | 22, 4 -> z_rot $* 3 $ x_rot
  | 22, 25 -> x_rot
  | 22, 49 -> z_rot $ x_rot
  | 22, 31 -> z_rot $* 2 $ x_rot
  | 49, 25 -> x_rot $* 2
  | 49, 22 -> y_rot $ x_rot $* 2
  | 49, 31 -> z_rot $* 2
  | 49, 28 -> y_rot $* 3 $ x_rot $* 2
  | 31, 4 -> z_rot $* 3
  | 31, 22 -> y_rot $ x_rot
  | 31, 49 -> z_rot $ x_rot $* 2
  | 31, 28 -> y_rot $* 3 $ x_rot $* 3
  | 28, 4 -> z_rot $* 3 $ x_rot $* 3
  | 28, 25 -> x_rot $* 3
  | 28, 49 -> z_rot $ x_rot $* 3
  | 28, 31 -> z_rot $* 2 $ x_rot $* 3
  | _ -> invalid_arg "get_cube_rot: cube is not a cube"

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

let cube_to_str (cube_perm: cube): string =
  let color (stick: int): char =
    let l = (stick / 3) in
    if l < 3 then 'U'
    else if l > 14 then 'D'
    else if l = 3 || l = 7 || l = 11 then 'F'
    else if l = 4 ||l = 8 || l = 12 then 'R'
    else if  l = 5 || l = 9 || l = 13 then 'B'
    else 'L'
  in
  List.fold_left (fun acc i -> acc ^ String.make 1 (color cube_perm.(i))) ""
  [0; 1; 2; 3; 4; 5; 6; 7; 8; (* UP *)
  12; 13; 14; 24; 25; 26; 36; 37; 38; (* RIGHT *)
  9; 10; 11; 21; 22; 23; 33; 34; 35; (* FRONT *)
  45; 46; 47; 48; 49; 50; 51; 52; 53; (* DOWN *)
  18; 19; 20; 30; 31; 32; 42; 43; 44; (* LEFT *)
  15; 16; 17; 27; 28; 29; 39; 40; 41 (* BACK *)
  ]

let realign (cube: cube): cube * cube_rot =
  let rot = get_cube_rot cube in
  rotate_cube cube (inverse_perm rot), rot

let is_solved_cube (c: cube): bool =
  let rec aux i =
    if i = Array.length c then true
    else c.(i) = i && aux (i+1)
  in
  aux 0

module CubeSet = Set.Make(struct type t = cube let compare = compare end)

let get_min_subgroup (generator: CubeSet.t): CubeSet.t =
  let rec aux (frontier: CubeSet.t) (explored: CubeSet.t) (card: int): CubeSet.t =
    let next = CubeSet.fold (fun c acc ->
        CubeSet.fold (fun c' acc ->
          CubeSet.add (multiply_cubes c c') acc
        ) acc generator
      ) frontier explored
    in
    let ncard = CubeSet.cardinal next in
    if ncard = card then explored
    else aux (CubeSet.diff next explored) next ncard
  in
  aux (CubeSet.singleton (get_id_cube ())) (CubeSet.singleton (get_id_cube ())) 1

let pll_subgroup =
  let f (s: string): cube =
    parse_alg s |> cube_from_alg
  in
  get_min_subgroup CubeSet.(singleton (f "U") |> add (f "R2B2RFR'B2RF'R") |> add (f "R2URUR'U'R'U'R'UR'"))

let () = print_int (CubeSet.cardinal pll_subgroup); print_newline ()
