(*
 * This file contains lots of CPS versions of common lists functions.
 * There are a lot of these, and you will likely not need most of them.
 *
 * You can likely solve the problem with almost exclusively using find and
 * append, but other solutions certainly exist, so we provide an extensive
 * collection of additional in case your solution needs one.
 *)


(*
 * map : ('a -> ('b -> 'c) -> 'c) -> 'a list -> ('b list -> 'c) -> 'c
 * REQUIRES: f x k valuable when x is valuable and k is total
 * ENSURES : map f L k == k (List.map (fn x => f x (fn y => y)) L)
 *)
fun map
  (f : 'a -> ('b -> 'c) -> 'c)
  (L : 'a list)
  (k : 'b list -> 'c)
  : 'c =
    (case L of
      [] => k []
    | x::xs => map f xs (fn xs' => f x (fn x' => k (x'::xs'))))


(*
 * mapPartial : ('a -> ('b -> 'c) -> (unit -> 'c) -> 'c)
 *                -> 'a list -> ('b list -> 'c) -> 'c
 * REQUIRES: f x sc fc valuable when x is valuable and sc, fc are total
 * ENSURES : mapPartial f L k == k L', where L' contains elements x' for all
 * x in L such that f x sc fc == sc x' for any sc : 'b -> 'c, fc : unit -> 'c
 *
 * This basically merges a map and a filter into one function
 *
 * The predicate/mapping function f is in CPS form, this means that it is
 * passed a value from the list, a success continuation, and a failure
 * continuation, and calls the success continuation if the predicate is
 * satisfied, failure otherwise. This allows us to pass in CPS functions as
 * predicate functions but also potentially lets us convert the element we
 * return from the predicate to be a different type. In this case, we convert
 * the input x : 'a to some x' : 'b.
 *)
fun mapPartial
  (f : 'a -> ('b -> 'c) -> (unit -> 'c) -> 'c)
  (L : 'a list)
  (k : 'b list -> 'c)
  : 'c =
    (case L of
      [] => k []
    | x::xs => mapPartial f xs (fn res => f x
        (fn x' => k (x'::res))
        (fn () => k res)))


(* mapPartial example

val L = [1, ~1, 2, ~2]
fun f x sc fc = if x > 0 then sc (2 * x) else fc ()
val k = MakeString.int_list

val () = Test.string ("mapPartial test", "[2, 4]", mapPartial f L k)
*)


(*
 * filter : ('a -> (unit -> 'b) -> (unit -> 'b) -> 'b)
            -> 'a list -> ('a list -> 'b) -> 'b
 * REQUIRES: p x sc fc valuable when x is valuable and sc, fc are total
 * ENSURES : filter p L k == k L', where L' is the list containing
 *    all elements x in L such that p x sc fc == sc () for any
 *    sc : unit -> 'b and fc : unit -> 'b
 *
 * The predicate function is in CPS form, this means that it is passed a value
 * from the list, a success continuation, and a failure continuation, and
 * calls the success continuation if the predicate is satisfied, failure
 * otherwise. This allows us to pass in CPS functions as predicate functions.
 *)
fun filter
  (p : 'a -> (unit -> 'b) -> (unit -> 'b) -> 'b)
  (L : 'a list)
  (k : 'a list -> 'b)
  : 'b =
    (case L of
      [] => k []
    | x::xs => filter p xs (fn res => p x
        (fn () => k (x::res))
        (fn () => k res)))

(* filter example

val L = [1, ~1, 2, ~2]
fun p x sc fc = if x > 0 then sc () else fc ()
val k = MakeString.int_list

val () = Test.string ("filter test", "[1, 2]", filter p L k)
*)


(*
 * foldl : ('a * 'b -> ('b -> 'c) -> 'c) -> 'b -> 'a list -> ('b -> 'c) -> 'c
 * REQUIRES: g (a, b) k' valuable when (a, b) is valuable and k' is total
 * ENSURES : foldl g z L k == k (List.foldl (fn (a, b) => g (a, b) (fn x => x)) z L)
 *
 * The idea is that the folding function g calls its own continuation with the
 * the updated accumulator. Useful for using CPS functions as g.
 *)
fun foldl
  (g : 'a * 'b -> ('b -> 'c) -> 'c)
  (z : 'b)
  (L : 'a list)
  (k : 'b -> 'c)
  : 'c =
    (case L of
      [] => k z
    | x::xs => g (x, z) (fn b => foldl g b xs k))


(*
 * foldr : ('a * 'b -> ('b -> 'c) -> 'c) -> 'b -> 'a list -> ('b -> 'c) -> 'c
 * REQUIRES: g (a, b) k' valuable when (a, b) is valuable and k' is total
 * ENSURES : foldr g z L k == k (List.foldr (fn (a, b) => g (a, b) (fn x => x)) z L)
 *
 * The idea is that the folding function g calls its own continuation with the
 * the updated accumulator. Useful for using CPS functions as g.
 *)
fun foldr
  (g : 'a * 'b -> ('b -> 'c) -> 'c)
  (z : 'b)
  (L : 'a list)
  (k : 'b -> 'c)
  : 'c =
    (case L of
      [] => k z
    | x::xs => foldr g z xs (fn b => g (x, b) k))


(*
 * revAppend : 'a list * 'a list -> ('a list -> 'b) -> 'b
 * REQUIRES: true
 * ENSURES : revAppend (L1, L2) k == k (List.revAppend (L1, L2))
 *)
fun revAppend (L1 : 'a list, L2 : 'a list) : ('a list -> 'b) -> 'b =
  foldl (fn (x,acc) => fn k => k (x::acc)) L2 L1


(*
 * rev : 'a list -> ('a list -> 'b) -> 'b
 * REQUIRES: true
 * ENSURES : rev L k == k (List.rev L)
 *)
fun rev (L : 'a list) : ('a list -> 'b) -> 'b = revAppend (L, [])


(*
 * append : 'a list * 'a list -> ('a list -> 'b) -> 'b
 * REQUIRES: true
 * ENSURES : (L1 @ L2) k == k (List.@ (L1, L2))
 *)
fun ((L1 : 'a list) @ (L2 : 'a list)) (k : 'a list -> 'b) : 'b =
  foldr (fn (x, acc) => fn k => k (x::acc)) L2 L1 k
val append = op@


(*
 * length : 'a list -> (int -> 'b) -> 'b
 * REQUIRES: true
 * ENSURES : length L k == k (List.length L)
 *)
fun length (L : 'a list) : (int -> 'b) -> 'b =
  foldl (fn (_, acc) => fn k => k (acc + 1)) 0 L


(*!
 *
 * find : ('a -> ('b -> 'c) -> (unit -> 'c) -> 'c)
 *     -> 'a list
 *     -> ('b -> 'c)
 *     -> (unit -> 'c)
 *     -> 'c
 *
 * REQUIRES: p x sc' fc' valuable when x is valuable and sc', fc' are total
 * ENSURES : find p L sc fc ==
 *    - sc b where b is some value if there exists some x in L such
 *      that p calls the success continuation with b
 *    - fc () if no such value x exists
 *
 * The predicate function is in CPS form, this means that it is passed a value
 * from the list, a success continuation, and a failure continuation, and
 * calls the success continuation if the predicate is satisfied, failure
 * otherwise. This allows us to pass in CPS functions as predicate functions
 * but also potentially lets us convert the element we return from the
 * predicate to be a different type.
 *)
fun find
  (p : 'a -> ('b -> 'c) -> (unit -> 'c) -> 'c)
  (L : 'a list)
  (sc : 'b -> 'c)
  (fc : unit -> 'c)
  : 'c =
    (case L of
      [] => fc ()
    | x::xs => p x sc (fn () => find p xs sc fc))
