structure ListNatural : NATURAL =
  struct
    type t = int list
    exception Nonpositive

    val one = [1]

    (* carry : digit * integer -> integer *)
    fun carry (0, ps) = ps
      | carry (c, []) = [c]
      | carry (c, p :: ps) =
        ((p + c) mod 10) :: carry ((p + c) div 10, ps)

    fun add ([], qs) = qs
      | add (ps, []) = ps
      | add (p :: ps, q :: qs) =
        ((p + q) mod 10) :: carry ((p + q) div 10, add (ps, qs))

    fun addition (x, y) = rev (add(rev x, rev y))

    (* Subtracts two digits taking into account any borrow *)
    fun subtractDigits(d1, d2, borrow) =
      let
        val diff = d1 - d2 - borrow
      in
        if diff < 0 then (diff + 10, 1) (* borrow from the next digit *)
        else (diff, 0) (* no need to borrow *)
      end

    (* Subtracts two lists of digits with the most significant bit on the left *)
    fun subtractLists([], [], 0) = []
      | subtractLists([], [], _) = raise Nonpositive
      | subtractLists([], y::ys, _) = raise Nonpositive
      | subtractLists(x::xs, [], borrow) =
          let
            val (newDigit, newBorrow) = subtractDigits(x, 0, borrow)
          in
            newDigit :: subtractLists(xs, [], newBorrow)
          end
      | subtractLists(x::xs, y::ys, borrow) =
          let
            val (newDigit, newBorrow) = subtractDigits(x, y, borrow)
          in
            newDigit :: subtractLists(xs, ys, newBorrow)
          end

    fun removeZeros (0::xs) = removeZeros xs
      | removeZeros [] = raise Nonpositive
      | removeZeros x = x

    fun subtraction (x, y) = (removeZeros o rev o subtractLists) (rev x, rev y, 0)
end