structure Matrices :> MATRICES =
struct
  (* elementop: ('a * 'a -> 'a) -> 'a Seq.t Seq.t -> 'a Seq.t Seq.t -> 'a Seq.t Seq.t
   * REQUIRES: binop is total, A, B are well-formed n x m matrices
   * ENSURES:
   *   if n = 0 or m = 0 then C is the "trivial" matrix
   *   otherwise C is a well-formed n x m matrix
   *   and forall i, j: binop (A[i][j], B[i][j]) ~= C[i][j]
   *  elementopt binop A B has O(nm) work for binop with O(1) work
   *  elementopt binop A B has O(1) span for binop with O(1) span
   *)
  fun elementop binop A B =
    Seq.zipWith (fn (rA, rB) => Seq.zipWith binop (rA, rB)) (A, B)

  (* transpose: 'a Seq.t Seq.t -> 'a Seq.t Seq.t
   * REQUIRES: S is a well-formed n x m matrix
   * ENSURES: transpose S ==> S' where
   *   if n = 0 then S' is the "trivial" matrix
   *   otherwise S' is a well-formed m x n matrix
   *   and forall i, j: S[i][j] ~= S'[j][i]
   *  transpose S has O(mn) work
   *  transpose S has O(1) span
   *)
  fun transpose xss =
    if Seq.null xss then Seq.empty ()
    else
      let
        val cols = Seq.length (Seq.nth xss 0)
      in
        Seq.tabulate (fn i => Seq.map (fn r => Seq.nth r i) xss) cols
      end

  (* multiply: int Seq.t Seq.t -> int Seq.t Seq.t -> int Seq.t Seq.t
   * REQUIRES: A is a well-formed n x m matrix,
   *   B is a well-formed m x p matrix
   * ENSURES: multiply A B ==> C where
   *   if n = 0, m = 0, or p = 0 then C is the "trivial" matrix
   *   otherwise C is a well-formed n x p matrix
   *   and forall i, j: sum from k = 1 to m of A[i][k]B[k][j] ~= C[i][j]
   *  multiply A B has O(nmp) work
   *  multiply A B has O(log m) span
   *)
  fun multiply A B =
    if Seq.null A orelse Seq.null B then Seq.empty ()
    else
      let
        val B_T = transpose B
        fun vprod u v = Seq.reduce (op +) 0 (Seq.zipWith (op *) (u, v))
      in
        Seq.map (fn rA => Seq.map (vprod rA) B_T) A
      end

end

fun fromListList L = Seq.fromList (List.map Seq.fromList L)
fun toListList S = Seq.toList (Seq.map Seq.toList S)

(* sample test from writeup *)
(* val M1 = fromListList [[1,2,3],[4,5,6]]
val () = Test.int_list_list_eq("test", [[1,4],[2,5],[3,6]], toListList (Matrices.transpose M1)) *)
