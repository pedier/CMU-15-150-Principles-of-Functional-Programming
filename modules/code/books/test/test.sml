
structure SlowBTest = MkTest (SlowBook)

val () = SlowBTest.test ()

structure BookTest = MkTest (Book)

val () = BookTest.test ()

(* Feel free use your test functor on other book structures you make *)
