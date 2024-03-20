open Permutation

type 'a abstr_alg = ('a * int) list

(* un mouvement élémentaire peut être interprété par le cube non orienté *)
type e_move = | F | U | D | B | R | L
type e_alg = e_move abstr_alg

type cube_rot = permutation (* of 6 elements*)
let id_rot () = id_perm 6

let x_rot = cycle 6 [|2; 0; 5; 3|]
let y_rot = cycle 6 [|1; 2; 4; 5|]
let z_rot = cycle 6 [|0; 4; 3; 1|]

(* single moves are
  | F | U | D | B | R | L
  | Rw=r | Lw=l | Uw=u | Dw=d | Fw=f | Bw=b
  | M | E | S
  | X | Y | Z
  
  any single move s verifies 4*s = id
  *)
type single_move = char

type alg = single_move abstr_alg
let id_alg = []

type cube = permutation (* of 54 elements *)

let inverse_alg (a: alg): alg =
  List.rev_map (fun (x, n) -> (x, 4-n)) a

let inverse_e_alg (a: e_alg): e_alg = 
  List.rev_map (fun (x, n) -> (x, 4-n)) a

let rec repeat_alg (a: alg) (n: int): alg = 
  if n = 0 then id_alg
  else if n = 1 then a
  else if n < 0 then repeat_alg (inverse_alg a) (-n)
  else
  match a with
  | [(_, p)] when (p * n) mod 4 = 0 -> []
  | [(x, p)] -> [(x, (p * n) mod 4)]
  | a -> a @ (repeat_alg a (n-1))

let compose_alg (a1: alg) (a2: alg): alg =
  a1 @ a2

(* ================================= PARSER ================================= *)

let is_num (c: char): bool = 
  match c with
  | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> true
  | _ -> false

exception ParseError of string * int

let parse_alg (s: string): alg =
  let rec get_closing_par (i: int) (j: int): int =
    let rec aux (i: int) (p: int): int =
      if i > j then i else
      match s.[i] with
      | '(' | '[' -> aux (i+1) (p+1)
      | ')' | ']' -> if p = 1 then i else aux (i+1) (p-1)
      | _ -> aux (i+1) p
    in aux i 0
  and n_end (i: int) (j: int): int =
    if i > j then (j + 1)
    else if is_num s.[i] then n_end (i+1) j
    else i

  in let rec parse_aux (i: int) (j: int): alg = 
    if i > j then []
    else let c = s.[i] in
    let (alg, i) =
      match c with
      | '(' | '[' ->
        let closing = get_closing_par i j in
        if closing > j then raise (ParseError ("Closing parenthesis not found", i))
        else parse_aux (i + 1) (closing - 1), closing + 1
      | 'x' | 'y' | 'z' | 'M' | 'E' | 'S' -> [(c, 1)], i+1
      | 'R' | 'L' | 'D' | 'U' | 'F' | 'B' ->
        if i+1 <= j && s.[i+1] = 'w'
          then [(Char.lowercase_ascii c, 1)], i+2
          else [(c, 1)], i+1
      | 'r' | 'l' | 'd' | 'u' | 'f' | 'b' -> [(c, 1)], i+1
      | ' ' -> [], i+1
      | a -> raise (ParseError ("Unrecognized character "^(String.make 1 a), i))
    in
    let (alg, i) = 
      if i <= j && s.[i] = '\''
        then (inverse_alg alg, i+1)
        else (alg, i)
    in
    let n_end_i = n_end i j in
    let alg =
      if n_end_i = i || i > j then alg
      else repeat_alg alg (int_of_string (String.sub s i (n_end_i - i)))
    in
    let (alg, i) = 
      if n_end_i <= j && s.[n_end_i] = '\''
        then (inverse_alg alg, n_end_i+1)
        else (alg, n_end_i)
    in
    let a = parse_aux i j in
    compose_alg alg a
  
  in parse_aux 0 (String.length s - 1)

(* ================================= ELEMENTALIZATION ================================= *)

let e_move_of_char (c: single_move): e_move =
  match c with
  | 'R' -> R | 'L' -> L | 'U' -> U | 'D' -> D | 'B' -> B | 'F' -> F
  | _ -> invalid_arg (String.make 1 c ^ " is not an elemental move")

let char_of_e_move (m: e_move): single_move =
  match m with
  | R -> 'R' | L -> 'L' | U -> 'U' | D -> 'D' | B -> 'B' | F -> 'F'

let rotate_e_move (m: e_move) (rot: cube_rot): e_move = 
  [|U; R; F; D; L; B|].(eval_perm rot 
    (match m with
    | U -> 0 | R -> 1 | F -> 2 | D -> 3 | L -> 4 | B -> 5)
  )

let rotate_e_alg (alg: e_alg) (rot: cube_rot): e_alg = 
  List.map (fun (m, n) -> (rotate_e_move m rot, n)) alg

let elementalize (a: alg) (rot: cube_rot): e_alg * cube_rot =
  let alg, rot = List.fold_left (fun (alg, rot) (m, n) ->
    match m with
    | 'x' -> alg, rot $ perm_pow x_rot n
    | 'y' -> alg, rot $ perm_pow y_rot n
    | 'z' -> alg, rot $ perm_pow z_rot n
    
    | 'M' -> (rotate_e_move R rot, n) :: (rotate_e_move L rot, 4-n) :: alg, rot $ perm_pow x_rot n
    | 'E' -> (rotate_e_move U rot, n) :: (rotate_e_move D rot, 4-n) :: alg, rot $ perm_pow y_rot n
    | 'S' -> (rotate_e_move B rot, n) :: (rotate_e_move F rot, 4-n) :: alg, rot $ perm_pow z_rot n
    
    | 'r' -> (rotate_e_move L rot, n) :: alg, rot $ perm_pow x_rot (-n)
    | 'l' -> (rotate_e_move R rot, n) :: alg, rot $ perm_pow x_rot (+n)
    | 'u' -> (rotate_e_move D rot, n) :: alg, rot $ perm_pow y_rot (-n)
    | 'd' -> (rotate_e_move U rot, n) :: alg, rot $ perm_pow y_rot (+n)
    | 'b' -> (rotate_e_move F rot, n) :: alg, rot $ perm_pow z_rot (-n)
    | 'f' -> (rotate_e_move B rot, n) :: alg, rot $ perm_pow z_rot (+n)
    
    | m -> (rotate_e_move (e_move_of_char m) rot, n) :: alg, rot
  ) (id_alg, rot) a
  in List.rev alg, rot

let elementalize_no_rot (a: alg): e_alg =
  elementalize a (id_perm 6) |> fst

let parse_e_alg (s: string): e_alg =
  elementalize_no_rot (parse_alg s)

let unelementalize (a: e_alg) (rot: cube_rot): alg =
  List.map (fun (m, n) -> char_of_e_move (rotate_e_move m rot), n) a

(* ================================= TO STRING ================================= *)

let alg_to_string ?(space = true) (a: alg): string =
  List.fold_left (fun (acc, sp) (x, n) -> 
    match n mod 4 with
    | 0 -> acc, space
    | 1 -> acc ^ (if sp then " " else "") ^ (String.make 1 (x)), space
    | 2 -> acc ^ (if sp then " " else "") ^ (String.make 1 (x)) ^ "2", space
    | 3 -> acc ^ (if sp then " " else "") ^ (String.make 1 (x)) ^ "'", space
    | _ -> assert false
  ) ("", false) a |> fst

let e_alg_to_string ?(space = true) (a: e_alg): string = 
  List.fold_left (fun (acc, sp) (x, n) -> 
    match n mod 4 with
    | 0 -> acc, space
    | 1 -> acc ^ (if sp then " " else "") ^ (String.make 1 (char_of_e_move x)), space
    | 2 -> acc ^ (if sp then " " else "") ^ (String.make 1 (char_of_e_move x)) ^ "2", space
    | 3 -> acc ^ (if sp then " " else "") ^ (String.make 1 (char_of_e_move x)) ^ "'", space
    | _ -> assert false
  ) ("", false) a |> fst

(* ================================= UTIL ================================= *)

let mirror (alg: alg): alg =
  List.map (function
    | ('R', n) -> ('L', 4-n)
    | ('L', n) -> ('R', 4-n)
    | ('r', n) -> ('l', 4-n)
    | ('l', n) -> ('r', 4-n)
    | ('U' | 'D' | 'E' | 'F' | 'B' | 'S' | 'y' | 'z' | 'u' | 'd' | 'f' | 'b' as x, n) -> (x, 4-n)
    | x -> x
    ) alg

let e_is_parallel (m1: e_move) (m2: e_move): bool =
  match m1, m2 with
  | U, D | R, L | F, B | D, U | L, R | B, F -> true
  | _ -> false

(* true if and only if m1 and m2 commute *)
let is_parallel (m1: single_move) (m2: single_move): bool =
  let aux m1 m2 =
    match m1, m2 with
    | ('x' | 'y' | 'z'), _ -> true
    | 'U', ('U' | 'D' | 'E' | 'u' | 'd') -> true
    | 'R', ('R' | 'L' | 'M' | 'r' | 'l') -> true
    | 'F', ('F' | 'B' | 'S' | 'f' | 'b') -> true
    | _ -> false
  in aux m1 m2 || aux m2 m1

let extract_alternate (alg: 'a abstr_alg) (m, n: 'a * int) (is_parallel: 'a -> 'a -> bool): 'a abstr_alg * ('a * int) =
  let rec aux alg acc (m, n) =
    match alg with
    | (m', n')::alg when is_parallel m m' ->
      if m' = m then aux alg acc (m, n+n')
      else aux alg ((m', n')::acc) (m, n)
    | alg -> List.rev_append acc alg, (m, n)
  in aux alg [] (m, n)

let rec remove_alternates (alg: 'a abstr_alg) (is_parallel: 'a -> 'a -> bool): 'a abstr_alg =
  match alg with
  | [] -> []
  | (m, n)::alg ->
    let alg, (m, n) = extract_alternate alg (m, n) is_parallel in
    (m, n mod 4)::remove_alternates alg is_parallel

let abstr_simplify (alg: 'a abstr_alg) (is_parallel: 'a -> 'a -> bool): 'a abstr_alg =
  remove_alternates alg is_parallel
  |> List.filter (fun (_, n) -> n <> 0)

let simplify (alg: alg): alg =
  abstr_simplify alg is_parallel

let e_simplify (alg: e_alg): e_alg =
  abstr_simplify alg e_is_parallel

let e_compose_simplified (a1: e_alg) (a2: e_alg): e_alg =
  let rec aux a1 acc =
    match a1 with
    | [] -> a2
    | [x] ->
      let a2, hd = extract_alternate a2 x e_is_parallel in
      List.rev_append acc (hd::a2)
    | x::a1 -> aux a1 (x::acc)
  in aux a1 []

let all_moves =
  ['F'; 'B'; 'D'; 'L'; 'U'; 'R'; 'f'; 'b'; 'd'; 'l'; 'u'; 'r'; 'S'; 'E'; 'M'; 'z'; 'y'; 'x']
  |> List.map (fun c -> [(c, 1); (c, 2); (c, 3)])
  |> List.flatten

let all_e_moves =
  [U; R; F; D; L; B]
  |> List.map (fun c -> [(c, 1); (c, 2); (c, 3)])
  |> List.flatten


(* ================================= CUBE FUNCTIONS ================================= *)

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
