datatype tree = Empty | Node of tree * int * tree

(* treeDifference : tree * tree -> int option
 * REQUIRES: true
 * ENSURES: treeDifference (T1, T2) ==> SOME x where x is the number of nodes where
              the values differ between T1 and T2 if T1 and T2 have the same structure
            OR treeDifference (T1, T2) ==> NONE if T1 and T2 do not have the same structure
*)
fun treeDifference (Empty : tree, Empty : tree) : int option = SOME 0
  | treeDifference (Empty : tree, Node _ : tree): int option = NONE
  | treeDifference (Node _ : tree, Empty : tree): int option = NONE
  | treeDifference (Node (L1, V1, R1) : tree, Node (L2, V2, R2) : tree) : int option =
      (case (treeDifference (L1, L2), treeDifference (R1, R2)) of
        (SOME leftDiff, SOME rightDiff) => SOME(leftDiff + rightDiff + (if V1 = V2 then 0 else 1))
        | _ => NONE)

val () = Test.int_option ("handout test 1", SOME 0, treeDifference (Node(Node(Node(Empty,2,Empty),5,Empty),4,Node(Empty,3,Node(Empty,1,Empty))), Node(Node(Node(Empty,2,Empty),5,Empty),4,Node(Empty,3,Node(Empty,1,Empty)))))
val () = Test.int_option ("handout test 2", SOME 3, treeDifference (Node(Node(Node(Empty,3,Empty),5,Empty),6,Node(Empty,3,Node(Empty,1,Empty))), Node(Node(Node(Empty,3,Empty),~5,Empty),6,Node(Empty,4,Node(Empty,2,Empty)))))
val () = Test.int_option ("handout test 3", NONE, treeDifference (Node(Node(Node(Empty,2,Empty),5,Node(Empty,0,Empty)),4,Node(Empty,3,Node(Empty,1,Empty))), Node(Node(Node(Empty,2,Empty),5,Empty),4,Node(Empty,3,Node(Empty,1,Empty)))))
val () = Test.int_option ("handout test 4", SOME 0, treeDifference (Empty, Empty))
val () = Test.int_option ("handout test 5", NONE, treeDifference (Empty, Node(Empty,1,Empty)))
val () = Test.int_option ("handout test 6", NONE, treeDifference (Node(Empty,1,Empty), Empty))
val () = Test.int_option ("additional test 1", SOME 2, treeDifference (Node(Node(Empty,1,Empty),2,Node(Empty,3,Empty)), Node(Node(Empty,4,Empty),2,Node(Empty,5,Empty))))
val () = Test.int_option ("additional test 2", NONE, treeDifference (Node(Empty,1,Empty), Node(Empty,1,Node(Empty,2,Empty))))
val () = Test.int_option ("additional test 3", SOME 1, treeDifference (Node(Empty,1,Node(Empty,2,Empty)), Node(Empty,1,Node(Empty,3,Empty))))
val () = Test.int_option ("additional test 4", SOME 0, treeDifference (Node(Empty,1,Empty), Node(Empty,1,Empty)))

