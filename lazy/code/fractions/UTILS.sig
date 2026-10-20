signature UTILS =
sig
  type contFraction
  type rational

  (* pi is a truncated continued fraction of pi
   * sourced from https://mathworld.wolfram.com/PiContinuedFraction.html
   *)
  val pi : contFraction

  (* phi is the continued fraction representation of phi *)
  val phi : contFraction

  (* sqrt2 is the continued fraction representation of sqrt(2) *)
  val sqrt2 : contFraction

  (* realEq : real -> real * real -> bool
   * REQUIRES: true
   * ENSURES: realEq p (x, y) = true if |x - y| < p, false otherwise
   *)
  val realEq : real -> real * real -> bool

  (* contFractionToList : contFraction -> int -> int list
   * REQUIRES: cf is productive and n >= 0
   * ENSURES: contFractionToList cf n = l where l is the list
   *          containing the first n coefficients of the stream representing cf
   *)
  val contFractionToList : contFraction -> int -> int list

  (* rationalToReal : rational -> real
   * REQUIRES: b <> 0
   * ENSURES: rationalToReal (a, b) evaluates to the real number represented by a / b
   *)
  val rationalToReal : rational -> real

  (* estimate : contFraction -> int -> rational
   * REQUIRES: cf is productive and n >= 0
   * ENSURES: estimate cf n evaluates to the the nth rational convergent of cf
   *          using your implementation of eval
   *)
  val estimate : contFraction -> int -> rational

end

