(* findPartitionV1 :
 *  'e list ->
 *  ('e list -> 'l option) ->
 *  ('e list -> 'r option) ->
 *  ('l * 'r) option
 * REQUIRES: pL and pR are total
 * ENSURES: findPartitionV1 A pL pR ==>*
 *  SOME (evL, evR) if there exists a partition (L, R) of A
 *  such that pL accepts L with evL and pR accepts R with evR.
 *  NONE otherwise.
 *)
fun findPartitionV1
  (A : 'e list)
  (pL : 'e list -> 'l option)
  (pR : 'e list -> 'r option)
  : ('l * 'r) option = case A of 
    [] => (case (pL [], pR []) of
             (SOME evL, SOME evR) => SOME (evL, evR)
           | _ => NONE)
  | x::xs => (case findPartitionV1 xs (fn L => pL (x::L)) pR of
               SOME res => SOME res
             | NONE => findPartitionV1 xs pL (fn R => pR (x::R)))

(* findPartitionV2 :
 *  'e list ->
 *  ('e list -> 'l option) ->
 *  ('e list -> 'r option) ->
 *  ('l * 'r -> 'a) ->
 *  (unit -> 'a) ->
 *  'a
 * REQUIRES: pL and pR are total
 * ENSURES: findPartitionV2 A pL pR sc fc ==>*
 *  sc (evL, evR) if there exists a partition (L, R) of A
 *  such that pL accepts L with evL and pR accepts R with evR.
 *  fc () otherwise.
 *)
fun findPartitionV2
  (A : 'e list)
  (pL : 'e list -> 'l option)
  (pR : 'e list -> 'r option)
  (sc : 'l * 'r -> 'a)
  (fc : unit -> 'a)
  : 'a = case A of 
    [] => (case (pL [], pR []) of
             (SOME evL, SOME evR) => sc (evL, evR)
           | _ => fc ())
  | x::xs => findPartitionV2 xs (fn L => pL (x::L)) pR sc 
      (fn () => findPartitionV2 xs pL (fn R => pR (x::R)) sc fc)


(* findPartition :
 *  'e list ->
 *  ('e list -> ('l -> 'a) -> (unit -> 'a) -> 'a) ->
 *  ('e list -> ('r -> 'a) -> (unit -> 'a) -> 'a) ->
 *  ('l * 'r -> 'a) ->
 *  (unit -> 'a) ->
 *  'a
 * REQUIRES: pL and pR are CPS-total
 * ENSURES: findPartition A pL pR sc fc ==>
 *  sc (evL, evR) if there exists a partition (L, R) of A
 *  such that pL accepts L with evL and pR accepts R with evR.
 *  fc () otherwise.
 *)
fun findPartition
  (A : 'e list)
  (pL : 'e list -> ('l -> 'a) -> (unit -> 'a) -> 'a)
  (pR : 'e list -> ('r -> 'a) -> (unit -> 'a) -> 'a)
  (sc : 'l * 'r -> 'a)
  (fc : unit -> 'a)
  : 'a = case A of 
    [] => pL [] (fn evL => pR [] (fn evR => sc (evL, evR)) fc) fc
  | x::xs => findPartition xs (fn L => pL (x::L)) pR sc 
      (fn () => findPartition xs pL (fn R => pR (x::R)) sc fc)
      

fun sum L k = foldr (fn (x, acc) => fn k' => k' (x + acc)) 0 L k


fun subsetSum (n : int) (xs : int list) : int list option =
  let
    fun pL L scL fcL = sum L (fn s => if s = n then scL L else fcL ())
    fun pR R scR fcR = scR ()
    fun sc (LJ, ()) = SOME LJ
    fun fc () = NONE
  in
    findPartition xs pL pR sc fc
  end


(* TESTS *)
val () = Test.int_list_option ("Test 1", SOME [], subsetSum 0 [])
val () = Test.int_list_option ("Test 2", NONE, subsetSum 1 [])
val () = Test.int_list_option ("Test 3", SOME [1], subsetSum 1 [1])
val () = Test.int_list_option ("Test 4", SOME [1,1], subsetSum 2 [1,1])
val () = Test.int_list_option ("Test 5", SOME [1,2], subsetSum 3 [1,2,1])
val () = Test.int_list_option ("Test 6", SOME [2,1], subsetSum 3 [5,2,1])