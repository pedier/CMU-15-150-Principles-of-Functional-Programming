structure ContFraction : CONTFRACTION =
struct
  (* values (a, b) of the rational type represent the rational
   * number a / b where a and b are coprime
   *)
  type rational = int * int

  (* the stream [a1, a2, a3, ...] represents the continued fraction
   * a1 + 1 / (a2 + 1 / (a3 + 1 / ...))
   *)
  type contFraction = int Stream.stream

  (* sqrt3 : unit -> contFraction
   * REQUIRES: true
   * ENSURES: sqrt3 () evaluates to the continued fraction of sqrt(3)
   *)
  fun sqrt3 () = Stream.delay (fn () => Stream.Cons (1, sqrt3' 1))
  and sqrt3' n =
    Stream.delay (fn () => Stream.Cons (n, sqrt3' (if n = 1 then 2 else 1)))

  (* euler : unit -> contFraction
   * REQUIRES: true
   * ENSURES: euler () evaluates to the continued fraction of euler's number
   *)
  fun euler () = euler' 0
  and euler' n =
    Stream.delay (fn () => Stream.Cons (an n, euler' (n + 1)))
  and an n = if n = 0 then 2 else if n mod 3 = 2 then 2 * ((n + 1) div 3) else 1

  (* generate : real -> contFraction
   * REQUIRES: true
   * ENSURES: generate r evaluates to the continued fraction representation of r
   *)
  fun generate r =
    Stream.delay (fn () => 
      let
        val a = Real.floor r
        val f = r - Real.fromInt a
      in
        if Real.== (f, 0.0) then Stream.Cons (a, Stream.empty)
          else Stream.Cons (a, generate (1.0 / f))
      end)

  (* eval : contFraction -> rational Stream.stream
   * REQUIRES: cf is productive
   * ENSURES: eval cf evaluates to the maximally lazy stream of well formed rational
   *          numbers where the nth element of the stream represents pn / qn
   *)
  fun eval cf = eval' cf (0, 1) (1, 0)
  and eval' cf (pn, qn) (pn1, qn1) =
    Stream.delay (fn () =>
      case Stream.expose cf of
        Stream.Empty => Stream.Empty
      | Stream.Cons (a, cf') =>
          let
            val pn2 = a * pn1 + pn
            val qn2 = a * qn1 + qn
          in
            Stream.Cons ((pn2, qn2), eval' cf' (pn1, qn1) (pn2, qn2))
          end)
end

