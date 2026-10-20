structure Book :> BOOK =
struct
    type page = string
    datatype direction = FORWARD | BACKWARD
    type book = page list * page list

    exception OutOfBounds

    fun bind L = ([], L)

    fun unbind (A, B) = List.rev A @ B

    fun currentPage ([], []) = (true, NONE)
      | currentPage ([], p :: _) = (true, SOME p)
      | currentPage (_, []) = (false, NONE)
      | currentPage (_, p :: _) = (false, SOME p)

    fun flip FORWARD (_, []) = raise OutOfBounds
      | flip FORWARD (A, p :: B) = (p :: A, B)
      | flip BACKWARD ([], _) = raise OutOfBounds
      | flip BACKWARD (p :: A, B) = (A, p :: B)
end

