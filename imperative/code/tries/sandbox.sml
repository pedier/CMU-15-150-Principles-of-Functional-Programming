(* printTrie : trie -> unit
 * REQUIRES: true is a well formated trie
 * ENSURES: printTrie trie ==> () and pretty prints the trie
 *)
fun printTrie (trie : Trie.trie) : unit =
  let
    fun nodeToString ch count = (String.str ch) ^ " (" ^ (Int.toString count) ^ ")"
    fun helper trie prefix isRoot isLastChild =
      let
        val Trie.Node (ch, count, children) = !trie
        val newPrefix =
            if isRoot then ""
            else prefix ^ (if isLastChild then "    " else "|   ")
        fun printChildren [] = ()
          | printChildren [child] = helper child newPrefix false true
          | printChildren (child::rest) = (helper child newPrefix false false; printChildren rest)
      in
        print (
          if isRoot then nodeToString ch count
          else prefix ^ (if isLastChild then "|-- " else "|-- ") ^ nodeToString ch count);
        print "\n";
        printChildren children
      end
  in helper trie "" true true end

(* trieEq : trie -> trie -> bool
 * REQUIRES: T1 and T2 are well formed tries
 * ENSURES: trieEq T1 T2 ==> true if T1 and T2 represent the same set of words, false otherwise
 *)
fun trieEq (T1 : Trie.trie) (T2 : Trie.trie) : bool =
  let
    val Trie.Node (char1, count1, children1) = !T1
    val Trie.Node (char2, count2, children2) = !T2
  in
    char1 = char2 andalso count1 = count2 andalso
    List.length children1 = List.length children2 andalso
    List.all (fn child1 =>
      let val Trie.Node (c, _, _) = !child1
      in
        case List.filter (fn child2 => let val Trie.Node (ch, _, _) = !child2 in ch = c end) children2 of
          [child2] => trieEq child1 child2
        | _ => false
      end
    ) children1
  end

(* addWords : trie -> string list -> unit
 * REQUIRES: trie is a well formed trie and all the strings in words contain
 *           only lowercase letters
 * ENSURES: addWords trie words ==> () and adds an occurence of each word in
 *          words into trie
 *)
fun addWords (trie : Trie.trie) (words : string list) : unit =
  List.app (Trie.addWord trie) words

(* checkCounts : trie -> (string * int) list -> unit
 * REQUIRES: trie is a well formed trie and all the strings in words contain
 *           only lowercase letters
 * ENSURES: checkCounts trie words ==> () and for each (word, count) in words,
 *          checks that Trie.findWord trie word ==> count. If not, an exception
 *          is raised
 *)
fun checkCounts (trie : Trie.trie) (words : (string * int) list) : unit =
  List.app (fn (word, count) => Test.int ("checking word count of " ^ word, count, Trie.findWord trie word)) words

(* Tests findWord individually by passing an input trie and testing if the counts returned by
 * findWord correctly match with the trie
 *)
fun findWordTests () =
  let
    val T1 =
      ref (Trie.Node (#"^", 0, [
        ref (Trie.Node (#"a", 1, [
          ref (Trie.Node (#"t", 3, [
            ref (Trie.Node (#"c", 1, [])),
            ref (Trie.Node (#"e", 1, []))
          ]))
        ])),
        ref (Trie.Node (#"b", 0, [
          ref (Trie.Node (#"a", 0, [
            ref (Trie.Node (#"t", 1, []))
          ])),
          ref (Trie.Node (#"o", 0, [
            ref (Trie.Node (#"t", 1, []))
          ]))
        ]))
      ]))

    val () = checkCounts T1
      [("a", 1), ("at", 3), ("atc", 1), ("ate", 1), ("bat", 1), ("bot", 1), ("batman", 0), ("c", 0), ("ated", 0)]

    (* WRITE YOUR OWN TESTS HERE *)
  in
    ()
  end

(* Tests addWord individually by adding words to the trie and checking if the resulting trie
 * matches the expected trie.
 *)
fun addWordTests () =
  let
    val expectedT =
      ref (Trie.Node (#"^", 0, [
        ref (Trie.Node (#"a", 1, [
          ref (Trie.Node (#"t", 3, [
            ref (Trie.Node (#"c", 1, [])),
            ref (Trie.Node (#"e", 1, []))
          ]))
        ])),
        ref (Trie.Node (#"b", 0, [
          ref (Trie.Node (#"a", 0, [
            ref (Trie.Node (#"t", 1, []))
          ])),
          ref (Trie.Node (#"o", 0, [
            ref (Trie.Node (#"t", 1, []))
          ]))
        ]))
      ]))

    val T1 = Trie.newTrie ()
    val () = addWords T1 ["a", "at", "at", "at", "atc", "ate", "bat", "bot"]
    val () = Test.bool ("addWord test ends up with the expected trie", trieEq expectedT T1, true)
  in
    ()
  end

(* Tests both your `addWord` and `findWord` functions together *)
fun allTests () =
  let
    val T1 = Trie.newTrie ()
    val () = addWords T1 ["a", "at", "at", "at", "atc", "ate", "bat", "bot"]
    val () = checkCounts T1
      [("a", 1), ("at", 3), ("atc", 1), ("ate", 1), ("bat", 1), ("bot", 1), ("batman", 0), ("c", 0), ("ated", 0)]

    val T2 = Trie.newTrie ()
    val () = addWords T2 ["polly", "values", "fun", "function", "valuable"]
    val () = checkCounts T2
      [("polly", 1), ("values", 1), ("fun", 1), ("function", 1), ("valuable", 1), ("value", 0), ("cps", 0)]
    val () = addWords T2 ["polly", "cps"]
    val () = checkCounts T2
      [("polly", 2), ("values", 1), ("fun", 1), ("function", 1), ("valuable", 1), ("value", 0), ("cps", 1)]

    val T3 = Trie.newTrie ()
    (* WRITE YOUR OWN TESTS HERE *)
  in
    ()
  end
