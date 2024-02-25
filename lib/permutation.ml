type permutation = int array

let eval_perm (p: permutation): int -> int = fun i -> if i < Array.length p then p.(i) else i

let id_perm n = Array.init n (fun i -> i)

let compose_perm (f: permutation) (g: permutation): permutation = 
  let size = max (Array.length f) (Array.length g)
  and f = eval_perm f
  and g = eval_perm g in
  Array.init size (fun n -> f (g n))

let ( $ ) = compose_perm

let inverse_perm (f: permutation): permutation = 
  let g = Array.make (Array.length f) (-1)
  in Array.iteri (fun i i' -> g.(i') <- i) f;
  g

let apply_perm (p: permutation) (a: 'a array): 'a array = 
  let p = eval_perm p in Array.init (Array.length a) (fun i -> a.(p i))

let cycle ?(n: int option) (c: int array): permutation =
  let cn = Array.length c in
  if cn = 0 then id_perm 0
  else
    let maxi = Array.fold_left (fun acc x -> max acc x) 0 c in
    let n =
      match n with
      | None -> maxi
      | Some n -> max maxi (n-1)
    in
    Array.init (n + 1) (fun i ->
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
