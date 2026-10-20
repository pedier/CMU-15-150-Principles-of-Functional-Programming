(* heads : int * int list -> int
 * REQUIRES: true
 * ENSURES: heads (x, xs) ==> the largest n such that the first n elements of xs
 * are all equal to x
 *)
fun heads (_ : int, [] : int list) : int = 0
  | heads (x : int, y::ys : int list) =
      if x = y then 1 + heads (x, ys) else 0

(* TEST CASES *)
(* Uncomment to test *)
val () = Test.int ("heads: test case 1", 2, heads (1,[1,1,2,1,3]))
val () = Test.int ("heads: test case 2", 0, heads (2,[1,1,2,1,3]))
(* Write more test cases here *)
val () = Test.int ("heads: test case 3", 3, heads (5,[5,5,5,1,2]))
val () = Test.int ("heads: test case 4", 0, heads (0,[]))
val () = Test.int ("heads: test case 5", 1, heads (1,[1,2,3]))
val () = Test.int ("heads: test case 6", 1, heads (1,[1,5,1,5,0]))


(* tails : int * int list -> int list
 * REQUIRES: true
 * ENSURES: tails (x, xs) ==> xs with the first k elements removed, where
 * k = heads (x, xs)
 *)
fun tails (_ : int, [] : int list) : int list = []
  | tails (x : int, y::ys : int list) =
      if x = y then tails (x, ys) else y::ys

(* TEST CASES *)
(* Uncomment to test *)
val () = Test.int_list_eq ("tails: test case 1", [2,1,3], tails (1,[1,1,2,1,3]))
val () = Test.int_list_eq ("tails: test case 2", [1,1,2,1,3], tails (2,[1,1,2,1,3]))
(* Write more test cases here *)
val () = Test.int_list_eq ("tails: test case 3", [1,2,3], tails (5,[5,5,5,1,2,3]))
val () = Test.int_list_eq ("tails: test case 4", [], tails (0,[]))
val () = Test.int_list_eq ("tails: test case 5", [2,3], tails (1,[1,2,3]))
val () = Test.int_list_eq ("tails: test case 6", [5,1,5,0], tails (1,[1,5,1,5,0]))


(* remove : int * int list -> int list
 * REQUIRES: true
 * ENSURES: remove (x, xs) ==> xs' such that xs' does not contain any element
 * y such that y = x, and xs' contains exactly all elements in xs except for x
 * in the same order as they were in xs
 *)
fun remove (_ : int, [] : int list) : int list = []
  | remove (x : int, y::ys : int list) =
      if x = y then remove (x, ys) else y::remove (x, ys)

(* TEST CASES *)
(* Uncomment to test *)
val () = Test.int_list_eq ("remove: test case 1", [2,3], remove (1,[1,1,2,1,3]))
val () = Test.int_list_eq ("remove: test case 2", [1,1,1,3], remove (2,[1,1,2,1,3]))
val () = Test.int_list_eq ("remove: test case 3", [1,5,1,5,0], remove (3,[1,5,1,5,0]))
(* Write more test cases here *)
val () = Test.int_list_eq ("remove: test case 4", [], remove (1,[1,1,1,1]))
val () = Test.int_list_eq ("remove: test case 5", [2,3,2], remove (4,[2,3,4,4,2]))


(* partition : int * int list -> int list * int list
 * REQUIRES: true
 * ENSURES: partition (pivot, L) ==> (L1, L2) such that
 *          for all x in L1, x <= pivot and
 *          for all y in L2, pivot < y.
 *)
fun partition (_ : int, [] : int list) : int list * int list = ([], [])
  | partition (pivot : int, x::xs : int list) =
      let val (small : int list, large : int list) = partition (pivot, xs)
      in if x > pivot then (small, x::large) else (x::small, large) 
      end

(* TEST CASES *)
(* Uncomment to test *)
val () =
  Test.int_list_int_list ("partition: test case 1", ([1,0],[3,4,5]), partition (2,[1,3,4,0,5]))
val () =
  Test.int_list_int_list ("partition: test case 2", ([1,2,3,0],[]), partition (3,[1,2,3,0]))
(* Write more test cases here *)
val () = 
  Test.int_list_int_list ("partition: test case 3", ([],[4,5,6]), partition (3,[4,5,6]))
val () =
  Test.int_list_int_list ("partition: test case 4", ([2,3,1],[5,4,6]), partition (3,[2,5,3,4,1,6]))
val () =
  Test.int_list_int_list ("partition: test case 5", ([],[]), partition (15150,[]))

