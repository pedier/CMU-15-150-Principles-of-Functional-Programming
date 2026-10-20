

(* updateClauseVar : var -> clause -> (unit -> 'a) -> (clause -> 'a) -> 'a
 * REQUIRES: true
 * ENSURES : updateClauseVar var clause sat update ==>
 *    - sat ()            if the assignment of var makes the clause satisfied
 *    - update clause'    otherwise, where clause' is clause with ~var removed
 *)
fun updateClauseVar
  (var : var)
  (clause : clause)
  (sat : unit -> 'a)
  (update : clause -> 'a)
  : 'a = case clause of
    [] => update []
  | x::xs => checkVar (x, var) (fn () => sat ())
      (fn () => updateClauseVar var xs sat update)
      (fn () => updateClauseVar var xs sat (fn xs' => update (x::xs')))


(* updateFormulaVar : var -> formula -> (formula -> 'a) -> 'a
 * REQUIRES: true
 * ENSURES : updateFormulaVar var formula k ==> k formula'
 * where formula' is the formula after assigning var
 *)
fun updateFormulaVar
  (var : var)
  (formula : formula)
  (k : formula -> 'a)
  : 'a = case formula of
    [] => k []
  | c::cs => updateClauseVar var c (fn () => updateFormulaVar var cs k)
      (fn c' => updateFormulaVar var cs (fn cs' => k (c'::cs')))


(* unitProp : formula -> (assignment * formula -> 'a) -> (unit -> 'a) -> 'a
 * REQUIRES: true
 * ENSURES : unitProp formula update unsat ==>
 *    - unsat ()                if through unit propagation, we can derive
 *                              the empty clause
 *    - update (asgn,formula')  otherwise, where asgn is the variables assigned
 *                              through unit propagation and formula' is
 *                              formula with no more singleton clauses and no
 *                              empty clauses
 *)
fun unitProp
  (formula : formula)
  (update : assignment * formula -> 'a)
  (unsat : unit -> 'a)
  : 'a = find isEmpty formula (fn () => unsat ())
    (fn () => find isSingleton formula
      (fn var => updateFormulaVar var formula 
        (fn f' => unitProp f' 
          (fn (asgn, f'') => update (var::asgn, f'')) unsat))
      (fn () => update ([], formula)))


