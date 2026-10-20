functor MkTest (Book : BOOK) :> TEST =
struct
  exception Error

  val test = fn () =>
    let
      val B0 = Book.bind []

      val () = (
        case Book.currentPage B0 of
          (true, NONE) => ()
        | _ => raise Error
      )

      val () = (
        case Book.unbind B0 of
          [] => ()
        | _ => raise Error
      )

      (* testing for unhandled Book.OutOfBounds exception *)
      val () = let val _ = Book.flip Book.FORWARD B0 in raise Error end handle Book.OutOfBounds => ()
      val () = let val _ = Book.flip Book.BACKWARD B0 in raise Error end handle Book.OutOfBounds => ()

      val B1 = Book.bind ["a"]
      val () =
        if Book.currentPage B1 = (true, SOME "a") then ()
        else raise Error
      val () =
        if Book.unbind B1 = ["a"] then ()
        else raise Error
      val () = let val _ = Book.flip Book.BACKWARD B1 in raise Error end handle Book.OutOfBounds => ()

      val B1f = Book.flip Book.FORWARD B1
      val () =
        if Book.currentPage B1f = (false, NONE) then ()
        else raise Error
      val () =
        if Book.unbind B1f = ["a"] then ()
        else raise Error
      val () = let val _ = Book.flip Book.FORWARD B1f in raise Error end handle Book.OutOfBounds => ()

      val B1fb = Book.flip Book.BACKWARD B1f
      val () =
        if Book.currentPage B1fb = (true, SOME "a") then ()
        else raise Error

      val B2 = Book.bind ["x", "y", "z"]
      val () =
        if Book.currentPage B2 = (true, SOME "x") then ()
        else raise Error
      val () =
        if Book.unbind B2 = ["x", "y", "z"] then ()
        else raise Error

      val B2f = Book.flip Book.FORWARD B2
      val () =
        if Book.currentPage B2f = (false, SOME "y") then ()
        else raise Error

      val B2ff = Book.flip Book.FORWARD B2f
      val () =
        if Book.currentPage B2ff = (false, SOME "z") then ()
        else raise Error

      val B2fff = Book.flip Book.FORWARD B2ff
      val () =
        if Book.currentPage B2fff = (false, NONE) then ()
        else raise Error
      val () =
        if Book.unbind B2fff = ["x", "y", "z"] then ()
        else raise Error
      val () = let val _ = Book.flip Book.FORWARD B2fff in raise Error end handle Book.OutOfBounds => ()

      val B2fffb = Book.flip Book.BACKWARD B2fff
      val () =
        if Book.currentPage B2fffb = (false, SOME "z") then ()
        else raise Error

      val B2fffbb = Book.flip Book.BACKWARD B2fffb
      val () =
        if Book.currentPage B2fffbb = (false, SOME "y") then ()
        else raise Error

      val B2fffbbb = Book.flip Book.BACKWARD B2fffbb
      val () =
        if Book.currentPage B2fffbbb = (true, SOME "x") then ()
        else raise Error
      val () = let val _ = Book.flip Book.BACKWARD B2fffbbb in raise Error end handle Book.OutOfBounds => ()
    in
      ()
    end
end
