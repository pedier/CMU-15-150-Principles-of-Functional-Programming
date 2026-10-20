(* This file contains a bunch of functions to help with testing code
 *
 * You shouldn't need to modify this file, instead you should write your tests
 * in the provided tests.sml
 *)

(*** STRING CONVERSION ***)

val varToString : var -> string = Int.toString
fun clauseToString (clause : clause) : string =
  case clause of
    [] => "F"
  | _  => String.concatWith " \\/ " (List.map varToString clause)
fun formulaToString (formula : formula) : string =
  case formula of
    [] => "T"
  | _  =>
      "   ("
      ^ String.concatWith ")\n/\\ (" (List.map clauseToString formula)
      ^ ")"

fun assignmentToString (asgn : assignment) : string =
  case asgn of
    [] => "-"
  | _  => String.concatWith " " (List.map varToString asgn)
(*** COMBINATORS ***)
(* The autograder is still using CM so we define these here *)
infix |>
infixr <|
fun x |> f = f x
fun f <| x = f x

(*** VALIDATING ASSIGNMENTS ***)
datatype result =
    SAT
  | UNSAT
  | SIMPLIFIED of formula (* used for partial assignments from Unit Prop *)

fun hasUnits (formula : formula) : bool =
  not (List.null (List.filter (fn [_] => true | _ => false) formula))
fun hasEmpty (formula : formula) : bool =
  not (List.null (List.filter List.null formula))

(* Tests whether two assignments are equal *)
fun asgn_eq (a1 : assignment, a2 : assignment) : bool =
  let
    val ord =
      ( ListMergeSort.uniqueSort Int.compare a1
      , ListMergeSort.uniqueSort Int.compare a2
      ) |> List.collate Int.compare
  in
    case ord of
      EQUAL => true
    | _     => false
  end

(* Tests whether two formulas are equal *)
fun formula_eq (f1 : formula, f2 : formula) : bool =
  let
    val (f1', f2') =
      ( List.map (ListMergeSort.uniqueSort Int.compare) f1
      , List.map (ListMergeSort.uniqueSort Int.compare) f2
      )
    (* technically allows duplicate clauses but they preserve satisfiability *)
    val isSubsetOf1 =
      List.all (fn cl2 => List.exists (fn cl1 => cl2 = cl1) f1') f2'
    val isSubsetOf2 =
      List.all (fn cl1 => List.exists (fn cl2 => cl1 = cl2) f2') f1'
  in
    (isSubsetOf1 andalso isSubsetOf2) orelse (hasEmpty f1 andalso hasEmpty f2)
  end

(* Tests whether two results are equivalent *)
val result_eq : result * result -> bool =
 fn (SAT, SAT) => true
  | (UNSAT, UNSAT) => true
  | (SIMPLIFIED f1, SIMPLIFIED f2) => formula_eq (f1, f2)
  | (_, _) => false

fun validAssign (asgn : assignment) : bool =
  not (List.exists (fn x => List.exists (fn y => x = ~y) asgn) asgn)

(* Computes an expected result given a formula and an assignment *)
fun checker (formula : formula) (asgn : assignment) : result =
  let
    (* Simplify the formula according to the assignment *)
    val simplified =
      List.foldl (fn (x, formula) =>
        List.mapPartial
          (fn cl => if List.exists (fn y => x = y) cl then NONE else SOME cl)
          formula
        |> List.map (List.filter (fn y => x <> ~y))
      ) formula asgn
  in
    case simplified of
      [] => SAT
    | _  =>
      (* DPLL should never enter the else clause *)
      if List.exists List.null simplified then UNSAT else SIMPLIFIED simplified
  end

local
  fun pass name = "PASS: " ^ name
  fun fail name = "FAIL: " ^ name
in
  (* Runs unitProp on the passed in tests and prints results. *)
  fun validateUnitProp' unitProp tests =
    let
      fun runTest (name, formula, _, e_unit_res) : string =
        let
          val res = unitProp formula SOME (fn _ => NONE)
        in
          case (e_unit_res, res) of
            (NONE, NONE) => pass name
          | (SOME (ea,ef), SOME (a,f)) =>
              if asgn_eq (ea, a) andalso formula_eq (ef,f)
              then pass name
              else fail name
          | _ => fail name
        end handle _ => fail name
    in
      print (String.concatWith "\n" (List.map runTest tests) ^ "\n")
    end

  (* Runs dpll on the passed in tests and prints results. *)
  fun validateDPLL' dpll tests =
    let
      fun runTest (name, formula, e_dpll_res, _) : string =
        let
          val res = dpll formula SOME (fn _ => NONE)
        in
          case res of
            SOME a =>
              if result_eq (checker formula a, e_dpll_res)
              then pass name
              else fail name
          | NONE =>
              if result_eq (UNSAT, e_dpll_res)
              then pass name
              else fail name
        end handle _ => fail name
    in
      print (String.concatWith "\n" (List.map runTest tests) ^ "\n")
    end
end

(*** MISC ***)
val use = fn _ =>
  print "WARNING: the use function only loads one file, and nothing else.\n\t\
        \This makes it relatively easy to accidentally not be using your \
        \updated function.\n\tInstead you should exit and re-enter the REPL. \
        \\n\n"
  (* before (use path) *)
