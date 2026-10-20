
(* dpll : formula -> (assignment -> 'a) -> (unit -> 'a) -> 'a
 * REQUIRES: true
 * ENSURES : dpll formula sat unsat ==>
 *    - unsat ()     if there is no satisfying assignment to formula
 *    - sat asgn     if there is a satisfying assignment to formula, asgn
 *)
fun dpll
  (cnf : formula)
  (sat : assignment -> 'a)
  (unsat : unit -> 'a)
  : 'a = unitProp cnf (fn (asgn, cnf') =>
    (case pickVar cnf' of
      NONE => sat asgn
    | SOME var => updateFormulaVar var cnf' 
        (fn cnf'' => dpll cnf'' 
          (fn asgn' => append (asgn, var::asgn') sat)
          (fn () => updateFormulaVar (~var) cnf'
            (fn cnf''' => dpll cnf''' 
              (fn asgn'' => append (asgn, (~var)::asgn'') sat) unsat)))))
    unsat