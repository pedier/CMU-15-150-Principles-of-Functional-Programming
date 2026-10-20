structure LazyTree : LAZYTREE =
struct
  datatype 'a lazyTree  = LT of unit -> 'a treeFront
  and      'a treeFront = LEAF | NODE of 'a lazyTree * 'a * 'a lazyTree

  val delay = LT
  fun expose (LT t) = t ()

  (* map: ('a -> 'b) -> 'a lazyTree -> 'b lazyTree
   * REQUIRES: true
   * ENSURES: map f t = t' where
   *     - t' has the same shape as t
   *     - Each x in t is replaced with f x in t'
   *     - map f t is maximally lazy (Evaluating map f t never calls f or exposes
   *       any elements of the tree)
   *)
  fun map f t = delay (fn () =>
    case expose t of
         LEAF => LEAF
       | NODE (l, x, r) => NODE (map f l, f x, map f r))

  (* preOrd: 'a lazyTree -> 'a Stream.t
   * REQUIRES: true
   * ENSURES: preOrd t = l where
   *     - l is the pre-order traversal for t (the pre-order traversal is the current
   *       node, then the left subtree, then the right subtree)
   *     - preOrd t is maximally lazy (Evaluating preOrd t does not expose any
   *       elements of the tree)
   *)
  fun preOrd t = Stream.delay (fn () => preOrd' [t])
  and preOrd' [] = Stream.Empty
    | preOrd' (t::ts) = case expose t of
         LEAF => preOrd' ts
       | NODE (l, x, r) => Stream.Cons (x, Stream.delay (fn () => preOrd' (l :: r :: ts)))
end
