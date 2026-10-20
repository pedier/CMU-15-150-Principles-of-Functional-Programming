functor MkReader (Student : STUDENT) :> READER =
struct
  structure Student = Student

  fun read b =
    let
      fun loop (x, book)=
        case Student.think (x, Book.currentPage book) of
          Student.YES => true
        | Student.NO => false
        | Student.MAYBE (x', dir) => loop (x', Book.flip dir book)
    in
      loop (Student.start, b)
    end
end

