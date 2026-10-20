type interval = int * int

(* all_available : interval list list -> interval list
 * REQUIRES:
 *   - Each person's obligations are disjoint
 *   - All schedules are well-formatted
 *   - Assume time starts at 0 and ends at the largest number given in the input
 *   - Assume that people can instantly transport themselves to and from commitments
 * ENSURES:
 *   all_available LL = L where L is a single list of all non-degenerate intervals
 *   in any order for which everyone is available.
 *)

fun all_available (LL : interval list list) : interval list =
  let
    val busy_set : interval list = List.concat LL  (* List.concat: O(m + n) *)

    fun cmp ((s1, e1) : interval, (s2, e2) : interval) : order =
      case ((s1 = s2), (e1 = e2)) of
        (false, _) => if s1 < s2 then LESS else GREATER
      | (true, false) => if e1 < e2 then LESS else GREATER
      | _ => EQUAL

    val sorted_busy_set : interval list = msort cmp busy_set  (* busy_set has size m, so calling msort costs O(m log m) *)

    fun merge_interval ((s, e) : interval, int_list : interval list) : interval list =
      (case int_list of
        [] => [(s, e)]
      | (s', e') :: res =>
        if s > e' then (s, e) :: int_list
        else (s', Int.max (e', e)) :: res)     (* s' <= s *)
    
    val merged : interval list = List.rev (List.foldl merge_interval [] sorted_busy_set)  
    (* sorted_busy_set has size at most m, so scanning it costs O(m) *)
    (* List.rev on a list of size at most m costs O(m) *)
    (* so for this step, total cost is O(m) *)
    
    val latest : int = List.foldl (fn ((_,e), mx) => Int.max(e, mx)) 0 busy_set
    (* Scanning busy_set of size m costs O(m) *)

    val (final_end : int, reversed_not_busy : interval list) = 
      List.foldl (fn ((s, e), (prev, temp)) =>
        if prev < s then (e, (prev, s) :: temp) else (e, temp)) (0, []) merged
    (* merged has size at most m, so scanning it costs O(m) *)
    
    val not_busy : interval list = List.rev reversed_not_busy
    (* reversed_not_busy has size at most m, so reverse it costs O(m) *)

    val fixed_not_busy : interval list = if final_end < latest then not_busy @ [(final_end, latest)] else not_busy
    (* not_busy has size at most m, so appending at the end costs O(m) *)
  in
    fixed_not_busy
  end


(* TESTS *)

val () =
  Test.list_eq (op =, MakeString.int_int)
    ("all_available_empty", [],
     all_available [])

val () =
  Test.list_eq (op =, MakeString.int_int)
    ("all_available_1",
     [(0,1),(3,4)],
     all_available [[(1,2),(4,6),(8,12)], [(1,3),(5,9)]])

val () =
  Test.list_eq (op =, MakeString.int_int)
    ("all_available_2",
     [(1,2),(3,5)],
     all_available [[(0,1),(2,3)], [(5,7)]])

val () =
  Test.list_eq (op =, MakeString.int_int)
    ("all_available_3",
     [(1,2)],
     all_available [[(0,1),(2,3)]])


(*
 * Work analysis:
 * List.concat costs O(m+n), the call to msort on m intervals costs O(m log m), 
 * and other steps are on list with size at most m, costing O(m). 
 * Since m <= N, total work is O(m log m + m + n) ⊆ O(m log m + N) ⊆ O(N log N + N) ⊆ O(N log N).
 *)