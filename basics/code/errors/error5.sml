(* pi : real *)
val pi : real = 3.14159265358979

(* vol : real -> real
 * REQUIRES: r >= 0
 * ENSURES: evaluates to the volume of a sphere with radius r *)
fun vol (r : real) : real = 4.0 / 3.0 * pi * r * r * r
