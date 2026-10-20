(*
 * This file contains lots of CPS versions of common lists functions.
 * There are a lot of these, and you will likely not need most of them.
 *
 * Not all of these functions are required to solve the problem, but they
 * may be useful to you. Feel free to use them if you find them helpful.
 * You do not need these functions until the nQueens problem, but you may
 * use them nonetheless.
 *
 * You can likely solve the problem with almost exclusively using length
 * and exists, but other solutions certainly exist, so we provide an
 * extensive collection of additional in case your solution needs one.
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
  foldr (fn (x, acc) => fn k' => k' (x::acc)) L2 L1 k
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


(*
  * exists : ('a -> (unit -> 'b) -> (unit -> 'b) -> 'b)
  *        -> 'a list
  *        -> (unit -> 'b)
  *        -> (unit -> 'b)
  *        -> 'b
  *
  * REQUIRES: p x sc' fc' valuable when x is valuable and sc', fc' are total
  * ENSURES : exists p L sc fc ==
  *    - sc () if there exists some x in L such that p x sc' fc' ==> sc' ()
  *    - fc () if for all x in L, p x sc' fc' ==> fc' ()
  *
  * This function is almost entirely identical to find, except that the success
  * continuation of the predicate always takes in unit instead of a polymorphic
  * type. This is because the predicate is only used to determine if an element
  * exists, without returning any useful information.
  *
  * Intuitively, this function checks to see if there exists an element in the
  * list that satisfies the predicate function p. If any element satisfies the
  * predicate, then the success continuation is called. If no element satisfies
  * the predicate, then the failure continuation is called.
 *)
fun exists
  (p : 'a -> (unit -> 'b) -> (unit -> 'b) -> 'b)
  (L : 'a list)
  (sc : unit -> 'b)
  (fc : unit -> 'b)
  : 'b =
    (case L of
      [] => fc ()
    | x::xs => p x sc (fn () => exists p xs sc fc))

(* exists example

val L = [2, ~2, 4, ~15150]
val L' = [1, 2, 3, 4]
fun p x sc fc = if x mod 2 == 1 then sc () else fc ()
val sc = fn () => "Exists odd"
val fc = fn () => "No odds"

val () = Test.string ("exists test1", "No odds", exists p L sc fc)
val () = Test.string ("exists test2", "Exists odd", exists p L' sc fc)

*)

(*
  * all : ('a -> (unit -> 'b) -> (unit -> 'b) -> 'b)
  *     -> 'a list
  *     -> (unit -> 'b)
  *     -> (unit -> 'b)
  *     -> 'b
  *
  * REQUIRES: p x sc' fc' valuable when x is valuable and sc', fc' are total
  * ENSURES : all p L sc fc ==
  *    - sc () if for all x in L, p x sc' fc' ==> sc' ()
  *    - fc () if there exists x in L such that p x sc' fc' ==> fc' ()
  *
  * The predicate function is in CPS form - see the documentation for find
  * for more information on what this means.
  *
  * Intuitively, this function checks to see if every element in the list
  * satisfies the predicate function p (where satisfying p means p x sc' fc'
  * calls sc' ()). If all elements satisfy the predicate, then the success
  * continuation is called. If any element does not satisfy the predicate, then
  * the failure continuation is called.
  *)
fun all
  (p : 'a -> (unit -> 'b) -> (unit -> 'b) -> 'b)
  (L : 'a list)
  (sc : unit -> 'b)
  (fc : unit -> 'b)
  : 'b =
    (case L of
      [] => sc ()
    | x::xs => p x (fn () => all p xs sc fc) fc)

(* all example

val L = [1, ~1, 2, ~2]
val L' = [1, 2, 3, 4]
fun p x sc fc = if x > 0 then sc () else fc ()
val sc = fn () => "All positive"
val fc = fn () => "Not all positive"

val () = Test.string ("all test1", "Not all positive", all p L sc fc)
val () = Test.string ("all test2", "All positive", all p L' sc fc)
 *)