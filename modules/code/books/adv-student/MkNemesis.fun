functor MkNemesis (Student : STUDENT) :> STUDENT =
struct
  type thoughts = Student.thoughts
  datatype status = YES | NO | MAYBE of thoughts * Book.direction

  val start = Student.start

  fun think (x, p) =
    case Student.think (x, p) of
      Student.YES => NO
    | Student.NO => YES
    | Student.MAYBE (x', dir) => MAYBE (x', dir)
end
