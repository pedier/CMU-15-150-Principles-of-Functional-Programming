datatype regexp =
    Zero
  | One
  | Char of char
  | Plus of regexp * regexp
  | Times of regexp * regexp
  | Star of regexp

(* This implementation of match also works on
 * regular expressions that aren't in standard form. *)
fun match (Zero) _ _ = false
  | match (One) cs k = k cs
  | match (Char(a)) cs k = (case cs of
                              nil => false
                            | (c::cs') => (a=c) andalso (k cs'))
  | match (Plus(r1,r2)) cs k = match r1 cs k orelse match r2 cs k
  | match (Times(r1,r2)) cs k = match r1 cs (fn cs' => match r2 cs' k)
  | match (Star(r)) cs k =
       k cs orelse
       match r cs (fn cs' => length cs' < length cs andalso match (Star(r)) cs' k)

fun accept r s = match r (String.explode s) List.null



type command_sequence = string  (* command sequence *)

(* example *)
val cmu_to_acrisure : command_sequence = "PB"

val downtown_loop_1 : command_sequence = "PR" 
val downtown_loop_2 : command_sequence = "BO"

val airport_to_kennywood_1 : command_sequence = "RGG"
val airport_to_kennywood_2 : command_sequence = "RBOGG"

local
  val R = Char #"R"
  val P = Char #"P"
  val B = Char #"B"
  val G = Char #"G"
  val O = Char #"O"
  val op + = Plus
  val op * = Times
in
  val downtown_to_downtown : regexp = Star ((P * R) + ((B * O) + ((G * P) + (G * G * G))))

  val airport_to_kennywood : regexp = R * downtown_to_downtown * G * G
end

