(* Write new test cases in this file! *)

(* Making nice printouts for the major functions *)
fun readable_unit cnf =
  unitProp cnf (fn (asgn, formula) =>
    "Unit Propagation\nAssigned:\n  "
    ^ assignmentToString asgn
    ^ "\nReturned Formula:\n"
    ^ (formulaToString formula)
    ^ "\n\n"
  ) (fn () => "Unit Propagation\nUNSATISFIABLE\n\n")
fun readable_dpll cnf =
  dpll cnf (fn asgn =>
    "DPLL\nSATISFIABLE\n  " ^ assignmentToString asgn ^ "\n\n"
  ) (fn () => "DPLL\nUNSATISFIABLE\n")

type test_unit_output = (assignment * formula) option
type testcase = (string * formula * result * test_unit_output)

(* Some sample test cases. Add more! *)
val empty_formula : formula = []
val empty_clause1 : formula = [[]]
val empty_clause2 : formula = [[1,2,~3], [~1,3], [], [~1]]
val one           : formula = [[1]]
val simple        : formula = [[1,~2], [2]]
val simple_part   : formula = [[1,~2], [2], [3,4]]
val no_unit       : formula = [[1,2], [2,3], [3,4], [5,6]]
val long_prop     : formula = [1]::List.tabulate (15, fn i => [~(i+1), (i+2)])
val not_long_prop : formula = List.map (List.map ~) long_prop
val long_no_prop  : formula = List.tabulate(15, fn i => [(i+1), (i+2), (i+3)])
val handout       : formula = [[1,~3,~5], [~1,2], [~1,~3,4], [~1,~2,3]]
val impossible    : formula = [[1], [~1]]
val impossible1   : formula =
  [[1,2,3],[1,2,~3],[1,~2,3],[1,~2,~3],[~1,2,3],[~1,2,~3],[~1,~2,3],[~1,~2,~3]]

(* test_unit_output = SOME (assignment * simplified formula) from unit prop
 *                  = NONE if conflict found
 *)
val empty_formula_out : test_unit_output = SOME ([],[])
val empty_clause1_out : test_unit_output = NONE
val empty_clause2_out : test_unit_output = NONE
val one_out           : test_unit_output = SOME ([1],[])
val simple_out        : test_unit_output = SOME ([1,2], [])
val simple_part_out   : test_unit_output = SOME ([1,2], [[3,4]])
val no_unit_out       : test_unit_output = SOME ([], no_unit)
val long_prop_out     : test_unit_output =
  SOME (List.tabulate (16, fn i => i + 1), [])
val not_long_prop_out : test_unit_output =
  SOME (List.tabulate (16, fn i => ~(i + 1)), [])
val long_no_prop_out  : test_unit_output = SOME ([], long_no_prop)
val handout_out       : test_unit_output = SOME ([], handout)
val impossible_out    : test_unit_output = NONE
val impossible1_out   : test_unit_output = SOME ([], impossible1)

(* list of: test name, formula, dpll satisfies, unit prop result *)
val tests : testcase list =
  [ ("empty_formula", empty_formula , SAT   , empty_formula_out)
  , ("empty_clause1", empty_clause1 , UNSAT , empty_clause1_out)
  , ("empty_clause2", empty_clause2 , UNSAT , empty_clause2_out)
  , ("one"          , one           , SAT   , one_out)
  , ("simple"       , simple        , SAT   , simple_out)
  , ("simple_part"  , simple_part   , SAT   , simple_part_out)
  , ("no_unit"      , no_unit       , SAT   , no_unit_out)
  , ("long_prop"    , long_prop     , SAT   , long_prop_out)
  , ("not_long_prop", not_long_prop , SAT   , not_long_prop_out)
  , ("long_no_prop" , long_no_prop  , SAT   , long_no_prop_out)
  , ("handout"      , handout       , SAT   , handout_out)
  , ("impossible"   , impossible    , UNSAT , impossible_out)
  , ("impossible1"  , impossible1   , UNSAT , impossible1_out)
  ] (* Add more! *)

(* Prints out the test cases, good for visual debugging. *)
local
  fun print_test driver (name, test, _, _) =
    print ("Test: " ^ name ^ "\n" ^ driver test)
in
  fun print_unit_tests tests = List.map (print_test readable_unit) tests
  fun print_dpll_tests tests = List.map (print_test readable_dpll) tests
end


(* Runs over the passed in test casses, printing whether they passed or not *)
val validateUnitProp : testcase list -> unit = validateUnitProp' unitProp
val validateDPLL : testcase list -> unit = validateDPLL' dpll
