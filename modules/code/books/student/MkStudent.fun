functor MkStudent (val count : int
                   val favWord : string) :> STUDENT =
struct
    type thoughts = int
    datatype status = YES | NO | MAYBE of thoughts * Book.direction
    val start : thoughts = 0
    fun think (_, (_, NONE)) = NO
        | think (x, (_, SOME w)) =
            if w = favWord then
                if x >= count - 1 then YES else MAYBE (x + 1, Book.FORWARD)
            else MAYBE (x, Book.FORWARD)
end
