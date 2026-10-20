(* e : real *)
val e : real = 2.71828182845905

(* isOne : int -> bool
 * REQUIRES: true
 * ENSURES: evaluates to true if the input is 1 and false otherwise *)
fun isOne (1 : int) = true
    | isOne _ = false

(* isPositive : int -> bool
 * REQUIRES: true
 * ENSURES: evaluates to true if the input is positive and false otherwise *)
fun isPositive (n : int) = n > 0
