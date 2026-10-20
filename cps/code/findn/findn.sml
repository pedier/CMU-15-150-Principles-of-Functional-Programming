datatype 'a shrub = Leaf of 'a
                  | Branch of 'a shrub * 'a shrub

(* findOne : ('a -> bool) -> 'a shrub -> ('a -> 'b) -> (unit -> 'b) -> 'b
 * REQUIRES: p is total
 * ENSURES:  findOne p T sc fc ==> sc x if x is in T and p x ==> true.
 *           If no such x exists, this evalutes to fc () instead.
 *           If more than one such x exists,
 *           findOne p T sc fc evaluates the leftmost such x.
 *)
fun findOne p (Leaf x) sc fc = if p x then sc x else fc ()
  | findOne p (Branch (l, r)) sc fc = findOne p l sc (fn () => findOne p r sc fc)


(* findTwo : ('a -> bool) -> ('a * 'a -> bool) -> 'a shrub
 *           -> ('a * 'a -> 'b) -> (unit -> 'b) -> 'b
 * REQUIRES: p is total, eq is total, eq represents an equivalence relation
 * ENSURES:  findTwo p eq T sc fc ==> sc (x, y) if x and y are values in T s.t.
 *           p x ==> true and p y ==> true and eq (x,y) ==> false.
 *           If no such pair (x,y) exists, findTwo p T sc fc ==> fc ()
 *)
fun findTwo p eq T sc fc = 
  findOne p T (fn v1 => 
    findOne (fn v2 => p v2 andalso not (eq (v1,v2))) T (fn v2 =>
      sc (v1, v2)) fc) fc


(* findN : ('a -> bool) -> ('a * 'a -> bool) -> 'a shrub -> int
 *         -> ('a list -> 'b) -> (unit -> 'b) -> 'b
 * REQUIRES: n >= 0, p is total, eq is total,
 *           eq represents an equivalence relation
 * ENSURES: findN p eq T n sc fc ==> sc L if there exists
 *          a list L of length n s.t the elements in L are pairwise distinct
 *          by eq, and for each element x in L, p x ==> true.
 *          Otherwise evaluates to fc ().
 *)
fun findN (p : 'a -> bool) (eq : 'a * 'a -> bool) (T : 'a shrub)
          (n : int) (sc : 'a list -> 'b) (fc : unit -> 'b) =
  (case n of
      0 => sc []
    | _ =>
        let
          fun success x = findN (fn y => p y andalso not (eq (x, y))) 
            eq T (n - 1) (fn xs => sc (x::xs)) fc
        in
          findOne p T success fc
        end)


(* TESTS *)
val T = Branch (Leaf 1 , Leaf 2)
val p = fn x => x > 0
val sc = SOME
val fc = fn () => NONE

val () = Test.int_option ("findOne p T", SOME 1, findOne p T sc fc)

val p0 = fn x => x = 0
val p2 = fn x => x = 2
val () = Test.int_option ("findOne p2 T ", SOME 2, findOne p2 T sc fc)
val () = Test.int_option ("findOne p0 T ", NONE, findOne p0 T sc fc)

val T1 = Branch (Leaf 1, Leaf 2)
val T2 = Branch (Leaf 1, Leaf 1)
val p1 = fn x => x > 0
val p2 = fn x => x = 1
val eq : int * int -> bool = op=
val sc = SOME
val fc = fn () => NONE

val () = Test.option (op =, MakeString.int_int)
  ("findTwo p1 T1", SOME (1, 2), findTwo p1 eq T1 sc fc)
val () = Test.option (op =, MakeString.int_int)
  ("findTwo p2 T1", NONE, findTwo p2 eq T1 sc fc)
val () = Test.option (op =, MakeString.int_int)
  ("findTwo p1 T2", NONE, findTwo p1 eq T2 sc fc)

val T = Branch (Leaf 1, Branch (Leaf 2, Leaf 3))
val p = fn x => x > 0
val eq : int * int -> bool = op=
val sc = SOME
val fc = fn () => NONE

val () = Test.option (op =, MakeString.int_list)
  ("findN p eq T 2", SOME [1, 2], findN p eq T 2 sc fc)
val () = Test.option (op =, MakeString.int_list)
  ("findN p eq T 3", SOME [1, 2, 3], findN p eq T 3 sc fc)
val () = Test.option (op =, MakeString.int_list)
  ("findN p eq T 4", NONE, findN p eq T 4 sc fc)
