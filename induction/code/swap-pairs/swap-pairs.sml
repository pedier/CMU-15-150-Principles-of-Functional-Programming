(* swapPairs: int list -> int list list
 * REQUIRES: true
 * ENSURES: swapPairs L ==> A list containing all the ways to swap exactly
 *          one adjacent pair of elements in L
 *)
fun swapPairs ([] : int list) : int list list = []
  | swapPairs ([_] : int list) : int list list = []
  | swapPairs (x::y::rest : int list) : int list list =
      let
        val swapped : int list = y :: x :: rest
        val remaining : int list list = swapPairs (y :: rest)

        fun add ([]: int list list) : int list list = []
          | add (l::ls : int list list) : int list list = (x :: l) :: add ls
      in
        swapped :: add remaining
      end

(* TEST CASES *)
(* Uncomment to test *)

(* Exactly one of the following two test cases should pass: *)
val () = Test.int_list_list_eq ("three elements", [[22, 50, ~4], [50, ~4, 22]], swapPairs [50, 22, ~4])
(* val () = Test.int_list_list_eq ("three elements", [[50, ~4, 22], [22, 50, ~4]], swapPairs [50, 22, ~4]) *)

(* Both of the following should pass: *)
val () = Test.int_list_list_eq ("two elements", [[5, 1]], swapPairs [1, 5])
val () = Test.int_list_list_eq ("one element", [], swapPairs [2])

(* Write more test cases here *)
val () = Test.int_list_list_eq ("four elements", [[2, 1, 3, 4], [1, 3, 2, 4], [1, 2, 4, 3]], swapPairs [1, 2, 3, 4])
val () = Test.int_list_list_eq ("no elements", [], swapPairs [])
val () = Test.int_list_list_eq ("five elements", [[5, 1, 1, 5, 0], [1, 1, 5, 5, 0], [1, 5, 5, 1, 0], [1, 5, 1, 0, 5]], swapPairs [1, 5, 1, 5, 0])
