(** the type for heaps *)
type 'a t

(** the exception raised when accessing an element in an empty heap *)
exception HeapEmpty

(** creates a new empty heap*)
val create : unit -> 'a t

val is_empty : 'a t -> bool

(** add an element to the heap, the float being the priority *)
val add : float -> 'a -> 'a t -> unit

(** returns the element of least priority and removes it from the heap *)
val take_min : 'a t -> float * 'a

(** returns the element of least priority without removing it from the heap *)
val peek_min : 'a t -> float * 'a

val cut_half : 'a t -> unit

val length : 'a t -> int
