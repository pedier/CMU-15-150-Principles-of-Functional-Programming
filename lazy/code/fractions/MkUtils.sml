functor MkUtils (CF : CONTFRACTION) : UTILS =
struct
  type contFraction = CF.contFraction
  type rational = CF.rational

  (* pi is a truncated continued fraction of pi
   * sourced from https://mathworld.wolfram.com/PiContinuedFraction.html
   *)
  val pi = Stream.delay (fn () =>
             Stream.Cons (3, Stream.delay (fn () =>
               Stream.Cons (7, Stream.delay (fn () =>
                 Stream.Cons (15, Stream.delay (fn () =>
                   Stream.Cons (1, Stream.delay (fn () =>
                     Stream.Cons (292, Stream.delay (fn () =>
                       Stream.Cons (1, Stream.delay (fn () =>
                         Stream.Cons (1, Stream.delay (fn () =>
                           Stream.Cons (1, Stream.delay (fn () =>
                             Stream.Cons (2, Stream.delay (fn () =>
                               Stream.Cons (1, Stream.delay (fn () =>
                                 Stream.Cons (3, Stream.delay (fn () =>
                                   Stream.Cons (1, Stream.delay (fn () =>
                                     Stream.Cons (14, Stream.delay (fn () =>
                                       Stream.Cons (2, Stream.delay (fn () =>
                                         Stream.Cons (1, Stream.delay (fn () => Stream.Empty)))))))))))))))))))))))))))))))

  (* phi is the continued fraction representation of phi *)
  val phi =
    let fun phi' () = Stream.delay (fn () => Stream.Cons (1, phi' ()))
    in phi' () end

  (* sqrt2 is the continued fraction representation of sqrt(2) *)
  val sqrt2 =
    let fun twos () = Stream.delay (fn () => Stream.Cons(2, twos ()))
    in Stream.delay (fn () => Stream.Cons (1, twos ())) end

  (* realEq : real -> real * real -> bool
   * REQUIRES: true
   * ENSURES: realEq p (x, y) = true if |x - y| < p, false otherwise
   *)
   fun realEq p (x, y) = Real.abs(x - y) < p

  (* contFractionToList : contFraction -> int -> int list
   * REQUIRES: cf is productive and n >= 0
   * ENSURES: contFractionToList cf n = l where l is the list
   *          containing the first n coefficients of the stream representing cf
   *)
  fun contFractionToList cf n = Stream.take (cf, n)

  (* rationalToReal : rational -> real
   * REQUIRES: b <> 0
   * ENSURES: rationalToReal (a, b) evaluates to the real number represented by a / b
   *)
  fun rationalToReal (a, b) = (Real.fromInt(a)) / (Real.fromInt(b))

  (* estimate : contFraction -> int -> rational
   * REQUIRES: cf is productive and n >= 0
   * ENSURES: estimate cf n evaluates to the the nth rational convergent of cf
   *          using your implementation of eval
   *)
  fun estimate cf n = List.nth (Stream.take (CF.eval cf, n + 1), n)
end
