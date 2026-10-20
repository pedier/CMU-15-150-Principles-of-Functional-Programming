structure Utils :> UTILS =
struct
  (* collect: ('a * 'a -> order) -> 'a Seq.t -> 'a Seq.t Seq.t
   * REQUIRES:
   *    cmp is "well-behaved"
   *    cmp is total
   *    cmp has O(1) work/span
   * ENSURES: collect cmp S ==> C where
   *    Every element in S appears in some sequence of C
   *    If you sum the lengths of the sequences in C, you get the length of S
   *    Elements in each sequence in C are cmp-EQUAL
   *    Elements in earlier sequences in C are cmp-LESS than elements in
   *      later sequences in C
   *    No sequence in C is empty
   *    collect cmp S has O(|S|log|S|) work
   *    collect cmp S has O(log^2|S|) span
   *)

  (* denote |S| as n *)
  (* O(n log n) work, O(log^2 n) span *)
  fun collect cmp S =
    let
      (* O(n log n) work, O(log^2 n) span *)
      val sorted = Seq.sort cmp S

      (* O(1) work, O(1) span *)
      val length = Seq.length sorted
    in
      (* O(1) work, O(1) span *)
      if length = 0 then
        (* O(1) work, O(1) span *)
        Seq.empty ()
      else
        let
          (* O(n) work, O(log n) span *)
          val heads =
            (* O(n) work, O(log n) span *)
            Seq.filter
              (* O(1) work, O(1) span *)
              (fn i => i = 0 orelse
                (* O(1) work, O(1) span *)
                (case cmp (Seq.nth sorted (i - 1), Seq.nth sorted i) of 
                  (* O(1) work, O(1) span *)
                  EQUAL => false | _ => true))
              (* O(n) work, O(1) span *)
              (Seq.tabulate (fn i => i) length)

          (* O(1) work, O(1) span *)
          val len_head = Seq.length heads
        in
          (* O(len_head) <= O(n) work, O(1) span *)
          Seq.tabulate
            (* O(1) work, O(1) span *)
            (fn j =>
              let
                (* O(1) work, O(1) span *)
                val lower = Seq.nth heads j

                (* O(1) work, O(1) span *)
                val upper = if j = len_head - 1 then length 
                                  (* O(1) work, O(1) span *)
                                  else Seq.nth heads (j + 1)
              in
                (* O(1) work, O(1) span *)
                Seq.subseq sorted (lower, upper - lower)
              end)
            len_head
        end
    end

  (* unique: ('a * 'a -> order) -> 'a Seq.t -> 'a Seq.t
   * REQUIRES:
   *    cmp is "well-behaved" (it is reflexive, anti-symmetric, and transitive)
   *    cmp is total and has O(1) work and span
   * ENSURES: unique cmp S ==> T where
   *    Each element of S has an EQUAL element in T
   *    The elements of T are in strictly increasing order (this prevents
   *      the existence of duplicates)
   *    unique cmp S has O(|S| log |S|) work and O(log^2 |S|) span
   *)

  fun unique cmp S =
    let
      (* O(|S| log |S|) work, O(log^2 |S|) span *)
      val groups = collect cmp S
    in
      (* O(|groups|) <= O(|S|) work, O(1) span *)
      Seq.map
      (* O(1) work, O(1) span *)
      (fn bucket => Seq.nth bucket 0) groups
    end
end
