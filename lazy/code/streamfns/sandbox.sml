(* Writeup tests *)

(* concat *)
val s = (Stream.fromList o List.map Stream.fromList) [[], [1, 5], [1], [], [5, 0]]
val () = Test.int_list_eq("test", [1,5,1,5,0], Stream.toList (StreamFns.concat s))

(* cycle *)
val s = Stream.fromList [1, 5, 0]
val () = Test.int_list_eq("test", [1, 5, 0, 1, 5, 0, 1, 5, 0, 1, 5], Stream.take (StreamFns.cycle s, 11))
val e : int Stream.t = Stream.delay (fn () => Stream.Empty)
val () = Test.int_list_eq("test", [], Stream.toList (StreamFns.cycle e))

(* interleave *)
val s1 = Stream.fromList ["functions", "are", "pointers"]
val s2 = Stream.fromList ["definitely", "not"]
val () = Test.string_list_eq("test", ["functions", "definitely",
    "are", "not", "pointers"],  Stream.toList (StreamFns.interleave s1 s2))

val s1 = Stream.fromList [1, 1, 5, 0]
val s2 = Stream.fromList [5]
val () = Test.int_list_eq("test", [1, 5, 1, 5, 0],
                          Stream.toList (StreamFns.interleave s1 s2))

val s1 : string Stream.t = Stream.fromList []
val s2 = Stream.fromList ["functions", "are", "values"]
val () = Test.string_list_eq("test", ["functions", "are", "values"],
                             Stream.toList (StreamFns.interleave s1 s2))

(* double *)

val s : int Stream.t = Stream.fromList []
val () = Test.int_list_eq("test", [], Stream.toList (StreamFns.double s))

val s = Stream.fromList [1, 2, 3]
val () = Test.int_list_eq("test", [1, 1, 2, 2, 3, 3],
                          Stream.toList (StreamFns.double s))

val s = Stream.fromList ["values", "values"]
val () = Test.string_list_eq("test", ["values", "values", "values", "values"], Stream.toList (StreamFns.double s))

(* add your own test cases! *)