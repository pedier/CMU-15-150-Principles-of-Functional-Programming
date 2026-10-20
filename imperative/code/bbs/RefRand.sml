structure RefRand : REFRAND =
struct
  type randstate = int * int ref

  fun init m seed = (m, ref seed)

  fun next (m, sref) = 
    let
      val x = (!sref * !sref) mod m
    in
      sref := x;
      x
    end
end

val r1 = RefRand.init 15 2
val () = Test.int ("test 1", 4, RefRand.next r1)
val r2 = RefRand.init 15 3
val () = Test.int ("test 2", 1, RefRand.next r1)
val () = Test.int ("test 3", 9, RefRand.next r2)
