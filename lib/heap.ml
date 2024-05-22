(* see https://github.com/mp2i-info/mp2i-library-ocaml/blob/master/src/lib/ds/priorityQueue/priorityQueue.ml *)

type 'a t = {mutable a : (float * 'a) option array; mutable n : int }

let pred i = (i - 1)/2
let g i = 2*i + 1
let d i = 2*i + 2

let my_compare (a: (float * 'a) option) (b: (float * 'a) option): int =
  match a, b with
  | None, None -> 0
  | None, _ -> 1
  | _, None -> -1
  | Some(x, _), Some(y, _) -> Float.compare x y

let swap h i j =
  let tmp = h.a.(i) in
  h.a.(i) <- h.a.(j);
  h.a.(j) <- tmp

let rec up heap i =
  let p = pred i in
  if i <> 0 && my_compare heap.a.(p) heap.a.(i) >= 0 then (
    swap heap i p;
    up heap p
  )

let my_min (a, j) (b, k) =
  if my_compare a b <= 0 then (a, j) else (b, k)

let rec down heap i =
  let m, j = my_min (let j = 2*i+1 in (if j < heap.n then heap.a.(j) else None), j)
                    (let j = 2*i+2 in (if j < heap.n then heap.a.(j) else None), j) in
  if my_compare heap.a.(i) m > 0 then (
    swap heap i j;
    down heap j
  )

let create () = { a = [||]; n = 0 }

let is_empty h = h.n = 0

let add f e heap =
  let prev_len = Array.length heap.a in
  if prev_len = heap.n then begin
    let array = Array.make (prev_len * 3 / 2 + 1) None in
    Array.blit heap.a 0 array 0 prev_len;
    heap.a <- array
  end;
  heap.a.(heap.n) <- Some(f, e);
  up heap heap.n;
  heap.n <- heap.n + 1

exception HeapEmpty

let take_min heap =
  if heap.n = 0 then raise HeapEmpty;
  swap heap 0 (heap.n - 1);
  heap.n <- heap.n - 1;
  down heap 0;
  heap.a.(heap.n) |> Option.get

let peek_min heap =
  if heap.n = 0 then raise HeapEmpty;
  heap.a.(0) |> Option.get

let cut_half heap =
  heap.n <- heap.n / 2

let length heap =
  heap.n
