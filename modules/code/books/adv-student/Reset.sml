structure Reset :> STUDENT =
struct
  type thoughts = unit
  datatype status = YES | NO | MAYBE of thoughts * Book.direction
  val start = ()
  fun think ((), (true, _)) = YES
    | think ((), (false, _)) = MAYBE ((), Book.BACKWARD)
end

