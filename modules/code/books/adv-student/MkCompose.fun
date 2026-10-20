functor MkCompose (structure StudentA : STUDENT
                   structure StudentB : STUDENT) :> STUDENT =
struct
  datatype thoughts = TA of StudentA.thoughts | TB of StudentB.thoughts
  datatype status = YES | NO | MAYBE of thoughts * Book.direction
  val start = TA StudentA.start
  fun think (TA x, p) =
        (case StudentA.think (x, p) of
           StudentA.YES =>
             (case StudentB.think (StudentB.start, p) of
                StudentB.YES => YES
              | StudentB.NO => NO
              | StudentB.MAYBE (y, dir) => MAYBE (TB y, dir))
         | StudentA.NO => NO
         | StudentA.MAYBE (x', dir) => MAYBE (TA x', dir))
    | think (TB x, p) =
        (case StudentB.think (x, p) of
           StudentB.YES => YES
         | StudentB.NO => NO
         | StudentB.MAYBE (x', dir) => MAYBE (TB x', dir))
end

