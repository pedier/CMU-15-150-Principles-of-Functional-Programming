datatype regexp =
    Zero
  | One
  | Char of char
  | Plus of regexp * regexp
  | Times of regexp * regexp
  | Star of regexp

(* acceptsEmpty: regexp -> bool
 * REQUIRES: true
 * ENSURES: acceptsEmpty r = accept r "" *)
fun acceptsEmpty (r : regexp) : bool =
  case r of
      Zero => false
    | One => true
    | Char _ => false
    | Plus (r1, r2) => acceptsEmpty r1 orelse acceptsEmpty r2
    | Times (r1, r2) => acceptsEmpty r1 andalso acceptsEmpty r2
    | Star _ => true

(* derivative: char * regexp -> regexp
 * REQUIRES: true
 * ENSURES: match (derivative (c, r)) cs List.null = match r (c::cs) List.null *)
fun derivative (c : char, r : regexp) : regexp =
  case r of
      Zero => Zero
    | One => Zero
    | Char x => if x = c then One else Zero
    | Plus (r1, r2) => Plus (derivative (c, r1), derivative (c, r2))
    | Times (r1, r2) =>
        if acceptsEmpty r1 then
          Plus (Times (derivative (c, r1), r2), derivative (c, r2))
        else Times (derivative (c, r1), r2)
    | Star r' => Times (derivative (c, r'), Star r')

(* newAccept : regexp -> string -> bool
 * REQUIRES: true
 * ENSURES: newAccept r s = accept r s *)
fun newAccept (r : regexp) (s : string) : bool =
  acceptsEmpty (List.foldl (fn (c, k) => derivative (c, k)) r (String.explode s))
