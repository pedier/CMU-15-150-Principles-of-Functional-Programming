(* fact : int -> int
 * REQUIRES: n >= 0
 * ENSURES: fact n evaluates to n factorial *)
fun fact (0 : int) : int = 1
  | fact n = n * fact (n - 1)

val result = fact 5
