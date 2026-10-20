signature CONTFRACTION =
sig
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
  val sqrt3 : unit -> contFraction

  (* euler : unit -> contFraction
   * REQUIRES: true
   * ENSURES: euler () evaluates to the continued fraction of euler's number
   *)
  val euler : unit -> contFraction

  (* generate : real -> contFraction
   * REQUIRES: true
   * ENSURES: generate r evaluates to the continued fraction representation of r
   *)
  val generate : real -> contFraction

  (* eval : contFraction -> rational Stream.stream
   * REQUIRES: cf is productive
   * ENSURES: eval cf evaluates to the stream of well formed rational numbers where the
   *          nth element of the stream represents pn / qn
   *)
  val eval : contFraction -> rational Stream.stream
end

