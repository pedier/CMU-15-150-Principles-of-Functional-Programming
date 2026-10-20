(* add : int * int -> int
 * REQUIRES: n, m >= 0
 * ENSURES: add (n, m) ==> n + m
 *)
fun add (0 : int, n : int) : int = n
  | add (m : int, n : int) : int = 1 + add (m - 1, n)

(* TEST CASES *)
val () = Test.int ("0 + 0", 0, add (0,0))
val () = Test.int ("0 + 5", 5, add (0,5))
val () = Test.int ("5 + 0", 5, add (5,0))
val () = Test.int ("15 + 150", 165, add (15,150))


(* mult : int * int -> int
 * REQUIRES: n, m >= 0
 * ENSURES: mult (m, n) ==> m * n
 *)
(* Note: You may want to pattern-match for different cases. *)
fun mult (0 : int, _ : int) : int = 0
  | mult (_ : int, 0 : int) : int = 0
  | mult (m : int, n : int) : int = add (n, mult (m - 1, n))
  

(* TEST CASES *)
(* Write test cases here *)
val () = Test.int ("0 * 0", 0, mult (0,0))
val () = Test.int ("0 * 15150", 0, mult (0,15150))
val () = Test.int ("15150 * 0", 0, mult (15150,0))
val () = Test.int ("1 * 15150", 15150, mult (1,15150))
val () = Test.int ("15150 * 1", 15150, mult (15150,1))
val () = Test.int ("151 * 50", 7550, mult (151,50))
val () = Test.int ("15 * 150", 2250, mult (15,150))

(* noDiv : int * int -> bool
 * REQUIRES: n > 1 and k >= 2
 * ENSURES: noDiv (n, k) ==> true if n is not divisible by any number from k to n-1 and false otherwise
 *)
fun noDiv (n : int, k : int) : bool =
  n = k orelse (not (n mod k = 0) andalso noDiv (n, k + 1))

(* TEST CASES *)
val () = Test.bool ("noDiv (2, 2)", true, noDiv (2, 2))
val () = Test.bool ("noDiv (7, 2)", true, noDiv (7, 2))
val () = Test.bool ("noDiv (9, 2)", false, noDiv (9, 2))
val () = Test.bool ("noDiv (13, 2)", true, noDiv (13, 2))
val () = Test.bool ("noDiv (15150, 2)", false, noDiv (15150, 2))
val () = Test.bool ("noDiv (15150, 15148)", true, noDiv (15150, 15148))



(* isPrime : int -> bool
 * REQUIRES: n > 1
 * ENSURES: isPrime n ==> true if n is prime and false otherwise
 *)
fun isPrime (n : int) : bool = noDiv (n, 2)

(* TEST CASES *)
(* Write test cases here *)
val () = Test.bool ("isPrime (2)", true, isPrime 2)
val () = Test.bool ("isPrime (7)", true, isPrime 7)
val () = Test.bool ("isPrime (41)", true, isPrime 41)
val () = Test.bool ("isPrime (9)", false, isPrime 9)
val () = Test.bool ("isPrime (10)", false, isPrime 10)
val () = Test.bool ("isPrime (15150)", false, isPrime 15150)