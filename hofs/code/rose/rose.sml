datatype 'a tree = Empty | Node of 'a tree * 'a * 'a tree
datatype 'a rose = Rose of 'a * 'a rose list

fun roseToString (f : 'a -> string) (Rose (x,rs)) : string =
  (f x) ^ "\n" ^ (
    case rs of
      [] => ""
    | _  =>
      MakeString.indent (
        String.concatWith "\n" (List.map (roseToString f) rs)
      )
  )

fun treeToRose (t : 'a tree) : 'a rose option =
  case t of
    Empty => NONE
  | Node (l, x, r) =>
      let
        val left : 'a rose list = (case treeToRose l of
                                      NONE => []
                                    | SOME left_rose => [left_rose]) 
        val right : 'a rose list = (case treeToRose r of
                                      NONE => []
                                    | SOME right_rose => [right_rose]) 
      in
         SOME (Rose (x, left @ right))
      end



val () = Test.test_eq (op=, MakeString.option (roseToString MakeString.int))
  ( "treeToRose0"
  , SOME (Rose (2, [Rose (1, []), Rose (3, [])]))
  , treeToRose (Node (Node (Empty, 1, Empty), 2, Node (Empty, 3, Empty)))
  )


fun roseMap (f : 'a -> 'b) (Rose (x, rs) : 'a rose) : 'b rose =
   Rose (f x, List.map (roseMap f) rs)

  


val () = Test.test_eq (op=, roseToString MakeString.int)
  ( "roseMap0"
  , Rose (4, [Rose (2, []), Rose (6, [])])
  , roseMap (fn x => x * 2) (Rose (2, [Rose (1, []), Rose (3, [])]))
  )


fun roseFoldl (f : 'a * 'b -> 'b) (z : 'b) (Rose (x, rs) : 'a rose) : 'b =
  List.foldl (fn (r, tmp) => roseFoldl f tmp r) (f (x, z)) rs


val () =
  Test.string
    ( "roseFoldl0"
    , "z213"
    , roseFoldl
        (fn (x, ac) => ac ^ Int.toString x)
        "z"
        (Rose (2, [Rose (1, []), Rose (3, [])]))
    )


fun roseFoldr (f : 'a * 'b -> 'b) (z : 'b) (Rose (x, rs) : 'a rose) : 'b =
  f (x, List.foldr (fn (r, tmp) => roseFoldr f tmp r) z rs)


val () =
  Test.string
    ( "roseFoldr0"
    , "z312"
    , roseFoldr
        (fn (x, ac) => ac ^ Int.toString x)
        "z"
        (Rose (2, [Rose (1, []), Rose (3, [])]))
    )

