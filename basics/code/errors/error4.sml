(* pi : real *)
val pi : real = 3.14159265358979

(* area : real -> real
 * REQUIRES: r >= 0
 * ENSURES: evaluates to the area of a circle with radius r *)
fun area (r : real) : real = pi * r * r
