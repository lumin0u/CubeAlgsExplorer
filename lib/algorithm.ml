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
