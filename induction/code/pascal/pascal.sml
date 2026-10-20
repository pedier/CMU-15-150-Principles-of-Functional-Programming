(* pascal : int * int -> int
 * REQUIRES: n >= k >= 0
 * pascal (n, k) ==> the element of Pascal's triangle at position (n,k)
 *)
fun pascal (_ : int, 0 : int) : int = 1
  | pascal (n : int, k : int) : int =
    if k = n then 1
    else pascal (n - 1, k - 1) + pascal (n - 1, k)

(* TEST CASES *)
(* Write test cases here *)
val () = Test.int ("pascal (0, 0)", 1, pascal (0,0))
val () = Test.int ("pascal (1, 0)", 1, pascal (1,0))
val () = Test.int ("pascal (1, 1)", 1, pascal (1,1))
val () = Test.int ("pascal (2, 0)", 1, pascal (2,0))
val () = Test.int ("pascal (2, 1)", 2, pascal (2,1))
val () = Test.int ("pascal (2, 2)", 1, pascal (2,2))
val () = Test.int ("pascal (3, 0)", 1, pascal (3,0))
val () = Test.int ("pascal (3, 1)", 3, pascal (3,1))
val () = Test.int ("pascal (3, 2)", 3, pascal (3,2))
val () = Test.int ("pascal (3, 3)", 1, pascal (3,3))
val () = Test.int ("pascal (4, 2)", 6, pascal (4,2))
val () = Test.int ("pascal (5, 2)", 10, pascal (5,2))
val () = Test.int ("pascal (5, 3)", 10, pascal (5,3))