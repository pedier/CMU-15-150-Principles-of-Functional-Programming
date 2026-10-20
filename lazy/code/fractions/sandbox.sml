(* SANDBOX to write tests *)

structure CF = ContFraction

structure Utils = MkUtils(CF)

fun testSqrt3 () =
  Test.int_list_eq ("test sqrt3", [1, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2], Utils.contFractionToList (CF.sqrt3 ()) 15)

fun testEuler () =
  Test.int_list_eq ("test euler", [2, 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, 1, 1, 10], Utils.contFractionToList (CF.euler ()) 15)

fun testGenerate () =
  let
    fun testGenerateHelper (name, expected, (r, elms)) =
      Test.int_list_eq (name, expected, Utils.contFractionToList (CF.generate r) elms)

    (* Increasing this value may cause failed generate tests even with
       correct generate implementations due to floating point errors *)
    val NUM_ELEMENTS = 20

    (* list of tuples: string * int list * (real * int)
       (test name, expected list of generated stream,
        (real number to generate, number of elements to take from stream)) *)
    val tests =
      [ ("test generate sqrt2",
        Utils.contFractionToList Utils.sqrt2 NUM_ELEMENTS,
        (Math.sqrt 2.0, NUM_ELEMENTS))
      , ("test generate using your sqrt3 implementation",
        Utils.contFractionToList (CF.sqrt3 ()) NUM_ELEMENTS,
        (Math.sqrt 3.0, NUM_ELEMENTS))
      , ("test generate using your euler implementation",
        Utils.contFractionToList (CF.euler ()) NUM_ELEMENTS,
        (Math.e, NUM_ELEMENTS))
      , ("test generate 1.23456789", [1, 4, 3, 1, 3, 1, 13565, 1, 8], (1.23456789, 9))
      , ("test generate ~2.1341451", [~3, 1, 6, 2, 5, 119, 1, 1, 1, 1], (~2.1341451, 10))
      , ("test generate 2.1341451", [2, 7, 2, 5, 119, 1, 1, 1, 1, 2], (2.1341451, 10))
      , ("test generate 31.43174713894123", [31,2,3,6,7,8,1,25,1,4], (31.43174713894123, 10))
      , ("test generate 7.77777777777", [7,1,3,2,1587272365], (7.77777777777, 5))
      , ("test generate 1.173729176281028", [1,5,1,3,10,49,2,2,17,1], (1.173729176281028, 10))
      , ("test generate 1.32391831", [1,3,11,2,7,2,1,8,5,1], (1.32391831, 10))
      , ("test generate sqrt14", [3,1,2,1,6,1,2,1,6,1], (Math.sqrt 14.0, 10))
      , ("test generate sqrt119", [10,1,9,1,20,1,9,1,20,1], (Math.sqrt 119.0, 10))
      , ("test generate sqrt666", [25,1,4,5,1,1,6,1,4,1,6,1,1,5,4,1,50], (Math.sqrt 666.0, 17))
      ]
  in List.app testGenerateHelper tests end

fun testEval () =
  let
    fun testEvalRationals (name, expected, actual) =
      Test.int_int (name, expected, actual)
    fun testEvalReal (name, expected, actual) =
      Test.test_eq (Utils.realEq 0.001, Real.toString) (name, expected, Utils.rationalToReal actual)

    (* list of tuples: string * rational * rational
       (test name, expected rational, actual output rational) *)
    val testRationals =
      [ ("test eval on pi", (245850922, 78256779), Utils.estimate (Utils.pi) 14)
      , ("test eval on phi (5)", (13, 8), Utils.estimate (Utils.phi) 5)
      , ("test eval on phi (10)", (144, 89), Utils.estimate (Utils.phi) 10)
      , ("test eval on phi (15)", (1597, 987), Utils.estimate (Utils.phi) 15)
      , ("test eval on phi (20)", (17711, 10946), Utils.estimate (Utils.phi) 20)
      , ("test eval on phi (25)", (196418, 121393), Utils.estimate (Utils.phi) 25)
      ]

    (* list of tuples: string * real * rational
       (test name, expected real, actual output rational) *)
    val testReals =
      [ ("test eval on sqrt2", Math.sqrt 2.0, Utils.estimate (Utils.sqrt2) 30)
      , ("test eval on your sqrt3 implementation", Math.sqrt 3.0, Utils.estimate (CF.sqrt3 ()) 30)
      , ("test eval on your euler implementation", Math.e, Utils.estimate (CF.euler ()) 30)
      ]
  in
    (List.app testEvalRationals testRationals; List.app testEvalReal testReals)
  end

fun testExtra () =
  (Test.test_eq (Utils.realEq 0.001, Real.toString)
                ("test generate and eval on decimal representation of pi",
                Utils.rationalToReal (Utils.estimate (CF.generate Math.pi) 15), Math.pi);
   Test.test_eq (Utils.realEq 0.001, Real.toString)
                ("test generate and eval on decimal representation of e",
                Utils.rationalToReal (Utils.estimate (CF.generate Math.e) 15), Math.e))

fun testAll () =
  (testSqrt3 (); testEuler (); testGenerate (); testEval (); testExtra ())

