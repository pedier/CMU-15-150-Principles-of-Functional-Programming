(*!
 * This is bad style. Maybe try pattern matching : )
 *)
fun valOf _  = raise Fail "Try pattern matching instead : )"
(*!
 * This is bad style. Maybe try pattern matching : )
 *)
fun isSome _ = raise Fail "Try pattern matching instead : )"

structure Option =
struct
  (*!
   * `getOpt (opt, a)` returns `v` if `opt` is `SOME(v)`;
   *   otherwise it returns `a`.
   *)
  val getOpt : 'a option * 'a -> 'a = Option.getOpt
  (*!
   * `filter f a` returns `SOME(a)` if `f(a)` is `true` and `NONE` otherwise.
   *)
  val filter : ('a -> bool) -> 'a -> 'a option = Option.filter
  (*!
   * The `join` function maps `NONE` to `NONE` and `SOME(v)` to `v`.
   *)
  val join : 'a option option -> 'a option = Option.join
  (*!
   * `map f opt` maps `NONE` to `NONE` and `SOME(v)` to `SOME(f v)`.
   *)
  val map : ('a -> 'b) -> 'a option -> 'b option = Option.map
  (*!
   * `mapPartial f opt` maps `NONE` to `NONE` and `SOME(v)` to `f(v)`.
   * The expression `mapPartial f` is equivalent to `join o (map f)`.
   *)
  val mapPartial = Option.mapPartial
end
