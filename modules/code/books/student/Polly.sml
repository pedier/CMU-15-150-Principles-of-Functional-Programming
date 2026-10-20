structure Polly :> STUDENT =
struct
  type thoughts = int
  datatype status = YES | NO | MAYBE of thoughts * Book.direction
  val start : thoughts = 0
  fun think (_, (_, NONE)) = NO
    | think (x, (_, SOME "lambda")) = if x >= 2 then YES else MAYBE (x + 1, Book.FORWARD)
    | think (x, (_, SOME _)) = MAYBE (x, Book.FORWARD)
end
