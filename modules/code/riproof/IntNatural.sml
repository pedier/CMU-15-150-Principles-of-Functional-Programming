structure IntNatural : NATURAL = struct
  type t = int
  exception Nonpositive

  val one = 1
  fun addition (x, y) = x + y
  fun subtraction (x,y) = if x <= y then raise Nonpositive else x - y
end