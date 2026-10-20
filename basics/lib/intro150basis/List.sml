
(*!
 * This is bad style. Maybe try pattern matching : )
 *)
fun hd _   = raise Fail "Try pattern matching instead : )"
(*!
 * This is bad style. Maybe try pattern matching : )
 *)
fun tl _   = raise Fail "Try pattern matching instead : )"
(*!
 * This is bad style. Maybe try pattern matching : )
 *)
fun null _ = raise Fail "Try pattern matching instead : )"

structure List =
struct
  (*!
   * `length l` returns the number of elements in the list `l`.
   *)
  val length : 'a list -> int = List.length
  (*!
   * `l1 @ l2` returns a list with `l1` appended to `l2`.
   *)
  val op@ : 'a list * 'a list -> 'a list = List.@
  (*!
   * `rev l` returns a list consisting of `l`'s elements in reverse order.
   *)
  val rev : 'a list -> 'a list = List.rev
  (*!
   * `tabulate (n, f)` returns a list of length `n` equal to
   *   `[f(0), f(1), ..., f(n-1)]`, created from left to right.
   *   It raises `Size` if `n < 0`.
   *)
  val tabulate : int * (int -> 'a) -> 'a list = List.tabulate
  (*!
   * `map f l` applies `f` to each element of `l` from left to right,
   *   returning the list of results.
   *)
  val map : ('a -> 'b) -> 'a list -> 'b list = List.map
  (*!
   * `mapPartial f l` applies `f` to each element of `l` from left to right,
   *   returning a list of results, with `SOME` stripped, where `f` was defined.
   *   `f` is not defined for an element of `l` if `f` applied to the element
   *   returns `NONE`.
   *
   * The above expression is equivalent to:
   *   `((map valOf) o (filter isSome) o (map f)) l`
   *)
  val mapPartial : ('a -> 'b option) -> 'a list -> 'b list = List.mapPartial
  (*!
   * `filter f l` applies `f` to each element `x` of `l`, from left to right,
   *   and returns the list of those `x` for which `f x` evaluated to `true`,
   *   in the same order as they occurred in the argument list.
   *)
  val filter : ('a -> bool) -> 'a list -> 'a list = List.filter
  (*!
   * `foldl f init [x1, x2, ..., xn]` returns `f(xn,...,f(x2, f(x1, init))...)`
   *   or `init` if the list is empty.
   *)
  val foldl : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b = List.foldl
  (*!
   * `foldr f init [x1, x2, ..., xn]` returns `f(x1 ,f(x2,..., f(xn, init)...))`
   *   or `init` if the list is empty.
   *)
  val foldr : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b = List.foldr
  (*!
   * This function combines two lists `l1` and `l2` into a list of pairs, with
   *   the first element of each list comprising the first element of the
   *   result, the second elements comprising the second element of the result,
   *   and so on. If the lists are of unequal lengths, `zip` ignores the excess
   *   elements from the tail of the longer one.
   *)
  val zip : 'a list * 'b list -> ('a * 'b) list = ListPair.zip
end
