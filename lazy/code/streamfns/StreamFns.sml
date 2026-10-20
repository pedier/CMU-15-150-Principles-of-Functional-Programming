structure StreamFns :> STREAMFNS =
struct
  structure S = Stream

  (* concat: 'a Stream.t Stream.t -> 'a Stream.t
   * REQUIRES: s is productive
   * ENSURES:
   *     - concat s = s', a stream containing the elements of s concatenated
   *       together.
   *     - concat s is maximally lazy (evaluating concat s never exposes any
   *       elements of s).
   *)
  fun concat ss = S.delay (fn () => concat' ss)
  and concat' ss =
    case S.expose ss of
      S.Empty => S.Empty
    | S.Cons (s, ss') => concatCurr s ss'
  and concatCurr s ss' =
    case S.expose s of
      S.Empty => S.expose (concat ss')
    | S.Cons (x, s') => S.Cons (x, concat (S.delay (fn () => S.Cons (s', ss'))))

  
  (* val h =
  (Stream.fromList o List.map Stream.fromList)
    [[], [1, 5], [1], [], [5, 0]]

  val () =
    Test.int_list_eq
      ("test", [1, 5, 1, 5, 0], Stream.toList (concat h)) *)

  (* cycle: 'a Stream.t -> 'a Stream.t
   * REQUIRES: s is productive
   * ENSURES:
   *     - cycle s is productive
   *     - cycle s = s', a stream formed by infinitely repeating the elements of s
   *     - if s = Stream.empty, then cycle s = Stream.empty
   *     - cycle s is maximally lazy (evaluating cycle s never exposes any
   *       elements of s).
   *)
  fun cycle s = cycle' s s
  and cycle' prev curr =
    S.delay (fn () =>
      case S.expose curr of
        S.Empty => (case S.expose prev of S.Empty => S.Empty
                      | S.Cons (x, prev') => S.Cons (x, cycle' prev prev'))
      | S.Cons (x, curr') => S.Cons (x, cycle' prev curr'))
  
  (* val m = Stream.fromList [1, 5, 0]

  val () =
    Test.int_list_eq
      ("test", [1, 5, 0, 1, 5, 0, 1, 5, 0, 1, 5], Stream.take (cycle m, 11))

  val e : int Stream.t =
    Stream.delay (fn () => Stream.Empty)

  val () =
    Test.int_list_eq
      ("test", [], Stream.toList (cycle e)) *)

  (* interleave: 'a Stream.t -> 'a Stream.t -> 'a Stream.t
   * REQUIRES: s1 and s2 are productive
   * ENSURES:
   *     - interleave s1 s2 is productive
   *     - interleave s1 s2 = s, where s contains alternating elements from s1
   *       and s2, starting with the first element of s1
   *     - If one stream ends early, the rest of the other stream is appended in its entirety
   *     - interleave s1 s2 is maximally lazy (evaluating interleave s1 s2
   *       never exposes any elements of s1 or s2).
   *)
  fun interleave s1 s2 = 
    S.delay (fn () => case S.expose s1 of
      S.Empty => S.expose s2
    | S.Cons (x, s1') => S.Cons (x, interleave s2 s1'))
  
  (* val s1 = Stream.fromList ["functions", "are", "pointers"]
  val s2 = Stream.fromList ["definitely", "not"]

  val () =
    Test.string_list_eq
      ("test",
      ["functions", "definitely", "are", "not", "pointers"],
      Stream.toList (interleave s1 s2))

  val s1 = Stream.fromList [1, 1, 5, 0]
  val s2 = Stream.fromList [5]

  val () =
    Test.int_list_eq
      ("test", [1, 5, 1, 5, 0], Stream.toList (interleave s1 s2))

  val s1 : string Stream.t = Stream.fromList []
  val s2 = Stream.fromList ["functions", "are", "values"]

  val () =
    Test.string_list_eq
      ("test",
      ["functions", "are", "values"],
      Stream.toList (interleave s1 s2)) *)

  (* double: 'a Stream.t -> 'a Stream.t
   * REQUIRES: s is productive
   * ENSURES:
   *     - double s is productive
   *     - double s = s', a stream containing the same elements as s, but with each
   *       element appearing twice in a row
   *     - double s is maximally lazy (evaluating double s never exposes any
   *       elements of s).
   *)
  fun double s = 
    S.delay (fn () => case S.expose s of
      S.Empty => S.Empty
    | S.Cons (x, s') => S.Cons (x, double2 x s'))
  and double2 x s' =
    S.delay (fn () => S.Cons (x, double s'))

  (* val n : int Stream.t = Stream.fromList []

  val () =
    Test.int_list_eq
      ("test", [], Stream.toList (double n))

  val n = Stream.fromList [1, 2, 3]

  val () =
    Test.int_list_eq
      ("test", [1, 1, 2, 2, 3, 3], Stream.toList (double n))

  val n = Stream.fromList ["values", "values"]

  val () =
    Test.string_list_eq
      ("test",
      ["values", "values", "values", "values"],
      Stream.toList (double n)) *)
  
end
