(* Use these and books and define your own to test your student structures *)

val B0 = Book.bind []

val B1 = Book.bind ["polly","honk"]

(* A dummy STUDENT structure with functor calls to help catch functor syntax mistakes *)
(* You can also use this to help implement your structure/functor templates *)

structure DUMMY :> STUDENT =
struct
    type thoughts = unit
    datatype status
    = YES
    | NO
    | MAYBE of thoughts * Book.direction

    val start = ()

    fun think _ = raise Fail "Unimplemented"
end

structure R = MkReader (DUMMY)
structure N = MkNemesis (DUMMY)
structure C = MkCompose (structure StudentA = DUMMY
                         structure StudentB = DUMMY)
