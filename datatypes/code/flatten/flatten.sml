(* flatten : int list list -> int list
 * REQUIRES: true
 * ENSURES: flatten LL ==>* F such that F is flattened with respect to LL
 *)
fun flatten ([] : int list list) : int list = []
  | flatten ([]::Ls : int list list) : int list = flatten Ls
  | flatten ((x::xs)::Ls : int list list) : int list = x::(flatten (xs::Ls))
