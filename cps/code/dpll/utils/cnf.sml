
type var = int (* the number ~1 indicates variable 1 is negated *)
type clause = var list (* a list of OR'd variables: [~1, 2] == ~1 \/ 2 *)
type formula = clause list (* conjunction (i.e. "and") of clauses *)

type assignment = var list

(*
 * These are some helper functions that we've provided for you, completely
 * valid solutions can use any number of these, so don't worry too much if you
 * don't use many, or use all of them, or any amount in between.
 *
 * WARNING: These helpers are CPS functions and may only be called in tail calls
 *)


(*
 * checkVar : var * var -> (unit -> 'a) -> (unit -> 'a) -> (unit -> 'a) -> 'a
 * REQUIRES: true
 * ENSURES : checkVar (x,y) same neg diff ==>
 *    - same ()    if x and y are the same variable
 *    - neg  ()    if x and y are the same variable but one is negated
 *    - diff ()    if x and y are different variables
 *)
fun checkVar
  (x : var, y : var)
  (same : unit -> 'a)
  (neg : unit -> 'a)
  (diff : unit -> 'a)
  : 'a =
    if x = y then same ()
    else if x = ~y then neg ()
    else diff ()


(*
 * varEq : var -> var -> (unit -> 'a) -> (unit -> 'a) -> 'a
 * REQUIRES: true
 * ENSURES : varEq x y sc fc ==>
 *    - sc ()     if x and y are the same variable
 *    - fc ()     otherwise
 *)
fun varEq (x : var) (y : var) (sc : unit -> 'a) (fc : unit -> 'a) : 'a =
  checkVar (x, y) sc fc fc


(*!
 * varNeg : var -> var -> (unit -> 'a) -> (unit -> 'a) -> 'a
 * REQUIRES: true
 * ENSURES : varNeg x y sc fc ==>
 *    - sc ()     if x and y are the same atom but one is negated
 *    - fc ()     otherwise
 *)
fun varNeg (x : var) (y : var) (sc : unit -> 'a) (fc : unit -> 'a) : 'a =
  checkVar (x, y) fc sc fc


(*!
 * isEmpty : clause -> (unit -> 'a) -> (unit -> 'a) -> 'a
 * REQUIRES: true
 * ENSURES : isEmpty clause sc fc ==>*
 *    - sc ()     if clause is the empty clause
 *    - fc ()     otherwise
 *)
fun isEmpty (clause : clause) (sc : unit -> 'a) (fc : unit -> 'a) : 'a =
  (case clause of
    [] => sc ()
  | _  => fc ())


(*!
 * isSingleton : clause -> (var -> 'a) -> (unit -> 'a) -> 'a
 * REQUIRES: true
 * ENSURES : isSingleton clause sc fc ==>
 *    - sc x      if clause contains exactly one variable, x
 *    - fc ()     otherwise
 *)
fun isSingleton (clause : clause) (sc : var -> 'a) (fc : unit -> 'a) : 'a =
  (case clause of
    [x] => sc x
  | _   => fc ())


(*
 * pickVar : formula -> var option
 * REQUIRES: the input does not contain the empty clause
 * ENSURES : pickVar formula ==>
 *    - NONE      if no variables exist in formula
 *    - SOME x    where x is some atom in formula
 *)
fun pickVar (formula : formula) : var option =
  (case formula of
    [] => NONE
  | []::_ => NONE (* REQUIRES violated, so should be unreachable. *)
  | (x::_)::_ => if x < 0 then SOME (~x) else SOME x)
