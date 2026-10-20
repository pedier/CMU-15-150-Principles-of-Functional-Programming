functor MkTracker (Utils: UTILS) :> TRACKER =
struct

  (* you may assume this is a refsol for utils *)
  val unique = Utils.unique

  type mascot = int
  type chat = mascot Seq.t
  (* tracker: a sequence of rows with length n. For each row i, it contains
   * the sequence of mascots that mascot i can reach in one step. Note that each
   * row i must be sorted and contain i itself. There should not be duplicates
   * in any row.
   *)
  type tracker = chat Seq.t

  (* empty: int -> tracker
   * REQUIRES:
   *    n >= 0
   * ENSURES:
   *    empty n reflects that there are no group chats created and a mascot
   *      index is from 0 to n-1. This representation should support all
   *      calls to addChat and query
   *)
  (* O(n) work, O(1) span *)
  fun empty (n : int) : tracker =
    (* O(n) work, O(1) span *)
    Seq.tabulate 
    (* O(1) work, O(1) span *)
    Seq.singleton n

  (* addChat: tracker -> chat -> tracker
   * REQUIRES:
   *    Every member of the new chat has an index in 0 to n-1
   *    The chat has no duplicates
   *    T can be produced through calls to empty and addChat
   * ENSURES:
   *    addChat T C represents both C and the chats in T while maintaining the
   *      necessary invariants to give the right answer under future calls
   *      to addChat and query.
   *    O(|C| n log n) work and O(log^2 n) span.
   *)
  fun addChat (T : tracker) (C : chat) : tracker =
    let
      (* O(|C| log |C|) <= O(n log n) work, O(log^2 |C|) <= O(log^2 n) span *)
      val sorted = unique Int.compare C

      (* O(|C| n log n) work, O(log^2 n) span *)
      val update =
        (* O(|C| n log n) work, O(log^2 n) span *)
        Seq.map
          (fn x =>
            let
              (* O(1) work, O(1) span *)
              val old = Seq.nth T x

              (* O(n) work, O(1) span *)
              val append = Seq.append (old, sorted)

              (* O(n log n) work, O(log^2 n) span *)
              val new = unique Int.compare append
            in
              (* O(1) work, O(1) span *)
              (x, new)
            end)
          sorted
    in
      (* O(n + |C|) = O(n) work, O(1) span *)
      Seq.inject (T, update)
    end

  (* query: tracker -> mascot -> int -> mascot Seq.t
   * REQUIRES:
   *    The mascot has an index from 0 to n-1
   *    The resolution is non-negative
   *    T can be produced through calls to empty and addChat
   * ENSURES: query T m r ==> A
   *    A contains every mascot from 0 to n − 1 such that there exists mascots
   *      s0, s1, . . . , sr where s0 = m and for all 0 ≤ i < r, si and
   *      s(i+1) share a chat or si = s(i+1)
   *    O(r n^2 log n) work and O(r * log^2 n) span
   *)
  fun query (T : tracker) (m : mascot) (r : int) : mascot Seq.t =
    let
      (* O(n^2 log n) work, O(log^2 n) span *)
      fun onestep informed =
        let
          (* O(|informed|) <= O(n) work, O(1) span *)
          val rows = 
            (* O(|informed|) <= O(n) work, O(1) span *)
            Seq.map 
              (* O(1) work, O(1) span *)
              (Seq.nth T) informed

          (* O(n^2) work, O(log n) span *)
          val canreach = Seq.flatten rows
        in
          (* O(n^2 log n^2) = O(n^2 log n) work, O(log^2 n) span *)
          unique Int.compare canreach
        end

      (* O(k n^2 log n) work, O(k log^2 n) span *)
      fun reachin 0 informed =
            (* O(1) work, O(1) span *)
            informed
        (* O(k n^2 log n) work, O(k log^2 n) span *)
        | reachin k informed =
            (* O((k-1) n^2 log n) work, O((k-1) log^2 n) span *)
            reachin (k - 1) 
              (* O(n^2 log n) work, O(log^2 n) span *)
              (onestep informed)
    in
      (* O(r n^2 log n) work, O(r log^2 n) span *)
      reachin r
        (* O(1) work, O(1) span *)
        (Seq.singleton m)
    end
end
