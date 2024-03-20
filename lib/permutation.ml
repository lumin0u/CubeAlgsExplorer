type permutation = int array

let eval_perm (p: permutation): int -> int = fun i -> if i < Array.length p then p.(i) else i

let id_perm n = Array.init n (fun i -> i)

(** [f] and [g] must be of same size *)
let compose_perm (f: permutation) (g: permutation): permutation =
  let size = Array.length f in
  Array.init size (fun n -> f.(g.(n)))

let ( $ ) = compose_perm

let inverse_perm (f: permutation): permutation = 
  let g = Array.make (Array.length f) (-1)
  in Array.iteri (fun i i' -> g.(i') <- i) f;
  g

let apply_perm (p: permutation) (a: 'a array): 'a array = 
  let p = eval_perm p in Array.init (Array.length a) (fun i -> a.(p i))

let cycle (n: int) (c: int array): permutation =
  let cn = Array.length c in
  if cn = 0 then id_perm 0
  else
    Array.init n (fun i ->
      match Array.find_index ((=) i) c with
      | None -> i
      | Some i -> c.((i + 1) mod cn)
    )

let rec perm_pow (p: permutation) (i: int): permutation =
  if i < 0 then perm_pow (inverse_perm p) (-i)
  else if i = 0 then id_perm (Array.length p)
  else if i = 1 then p
  else let q = perm_pow p (i / 2) in
  if i mod 2 = 0 then
    q $ q
  else q $ q $ p

let ( $* ) = perm_pow
