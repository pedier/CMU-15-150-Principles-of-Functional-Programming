signature TRIE =
sig
  type trie
  datatype cell = Node of char * int * trie list

  val newTrie : unit -> trie

  val findWord : trie -> string -> int
  val addWord : trie -> string -> unit
end
