signature NATURAL =
  sig
    type t
    exception Nonpositive

    val one : t
    val addition : t * t -> t
    val subtraction : t * t -> t
  end