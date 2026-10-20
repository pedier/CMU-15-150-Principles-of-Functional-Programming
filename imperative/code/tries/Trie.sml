structure Trie : TRIE =
struct
  datatype cell = Node of char * int * trie list
  withtype trie = cell ref

  (* emptyTrie : unit -> trie
   * REQUIRES: true
   * ENSURES: newTrie () ==> trie, where `trie` is the empty trie.
   *)
  fun newTrie () = ref (Node (#"^", 0, []))

  (* findTrie : trie list -> char -> trie option
   * REQUIRES: tries does not include any nodes with duplicate characters
   * ENSURES: findTrie tries c ==> SOME trie where trie's character = c
                               ==> NONE if no such cell exists
   *)
  fun findTrie (tries : trie list) (c : char) : trie option =
    case List.filter (fn node => let val Node (ch, _, _) = !node in ch = c end) tries of
      [trie] => SOME trie
    | _ => NONE

  (* findWord : trie -> string -> int
   * REQUIRES: T is well formed, word is not empty and only contains lowercase letters
   * ENSURES: findWord T word ==> n where n is the number of times the word
   *          word has been added to T. Note that n is 0 when word is not in T.
   *)
  fun findWord (T : trie) (word : string) : int = 
    let
      fun findWord' _ [] = 0
        | findWord' curr [c] = 
            let
              val Node (_, _, next) = !curr
            in
              case findTrie next c of
                NONE => 0
              | SOME t =>
                  let 
                    val Node (_, cnt, _) = !t
                  in
                    cnt
                  end
            end
        | findWord' curr (c::cs) =
            let
              val Node (_, _, next) = !curr
            in
              case findTrie next c of
                NONE => 0
              | SOME t => findWord' t cs
            end
    in
      findWord' T (String.explode word)
    end

  (* addWord : trie -> string -> unit
   * REQUIRES: T is well formed, word is not empty and only contains lowercase letters
   * ENSURES: addWord T word ==> () and adds an occurence of word into T.
   *)
  fun addWord (T : trie) (word : string) : unit = 
    let
      fun addWord' _ [] = ()
        | addWord' curr [c] = 
            let
              val Node (ch, cnt, next) = !curr
            in
              case findTrie next c of
                SOME t =>
                  let
                    val Node (ch', cnt', next') = !t
                  in
                    t := Node (ch', cnt' + 1, next')
                  end
              | NONE =>
                  let 
                    val newNode = ref (Node (c, 1, []))
                  in
                    curr := Node (ch, cnt, newNode :: next)
                  end
            end
        | addWord' curr (c::cs) =
            let
              val Node (ch, cnt, next) = !curr
            in
              case findTrie next c of
                SOME t => addWord' t cs
              | NONE => 
                  let 
                    val newNode = ref (Node (c, 0, []))
                    val () = curr := Node (ch, cnt, newNode :: next)
                  in
                    addWord' newNode cs
                  end
            end
    in
      addWord' T (String.explode word)
    end

end
