type priority = float

type 'a t = Empty | Node of priority * 'a * 'a t * 'a t

let empty = Empty

let rec insert queue prio elt =
  match queue with
    Empty -> Node(prio, elt, Empty, Empty)
  | Node(p, e, left, right) ->
      if prio <= p
      then Node(prio, elt, insert right p e, left)
      else Node(p, e, insert right prio elt, left)

exception Queue_is_empty

let rec remove_top = function
    Empty -> raise Queue_is_empty
  | Node(_, _, left, Empty) -> left
  | Node(_, _, Empty, right) -> right
  | Node(_, _, (Node(lprio, lelt, _, _) as left),
                    (Node(rprio, relt, _, _) as right)) ->
      if lprio <= rprio
      then Node(lprio, lelt, remove_top left, right)
      else Node(rprio, relt, left, remove_top right)

let extract = function
    Empty -> raise Queue_is_empty
  | Node(prio, elt, _, _) as queue -> (prio, elt, remove_top queue)
