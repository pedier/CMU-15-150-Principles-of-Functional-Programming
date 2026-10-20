type vault = exn list


(* get      : 'a key -> vault -> 'a option
 * REQUIRES : M was produced from a valid sequence of operations for a vault
 * ENSURES  : get k M ==> SOME y   if there exists x in M such that
 *                                    decrypt k x ==> SOME y
 *                        NONE     otherwise
 *)
fun get (k : 'a key) (M : vault) : 'a option = 
  case M of
      [] => NONE
    | x :: xs =>
        case decrypt k x of
            SOME y => SOME y
          | NONE => get k xs

(* replace  : 'a key -> 'a -> vault -> 'a option * vault
 * REQUIRES : M was produced from a valid sequence of operations for a vault
 * ENSURES  : replace k v M ==> (SOME y, M')   if there exists x in M such that
 *                                                decrypt k x = SOME y and M'
 *                                                contains x' = encrypt k v
 *                                                instead of x
 *                              (NONE, M)      otherwise
 *)
fun replace (k : 'a key) (v : 'a) (M : vault) : 'a option * vault =
  case M of
      [] => (NONE, [])
    | x :: xs =>
        case decrypt k x of
            SOME y => (SOME y, (encrypt k v) :: xs)
          | NONE =>
              let
                val (res, xs') = replace k v xs 
              in
                (res, x :: xs')
              end

(* deposit  : 'a key -> 'a -> vault -> vault
 * REQUIRES : M was produced from a valid sequence of operations for a vault
 * ENSURES  : deposit k v M ==> M' such that M' contains x = encrypt k v,
 *            no other values x' = encrypt k v' (for any value v'),
 *            and all values y in M such that decrypt k y = NONE
 *)
fun deposit (k : 'a key) (v : 'a) (M : vault) : vault = 
  case M of
      [] => [encrypt k v]
    | x :: xs =>
        case decrypt k x of
            SOME _ => (encrypt k v) :: xs
          | NONE => x :: (deposit k v xs)


(* withdraw : 'a key -> vault -> vault
 * REQUIRES : M was produced from a valid sequence of operations for a vault
 * ENSURES  : withdraw k M ==> M' where M' contains all elements x of M
 *              such that decrypt k x ==> NONE (and no others)
 *)
fun withdraw (k : 'a key) (M : vault) : vault =
  case M of
      [] => []
    | x :: xs =>
        case decrypt k x of
            SOME _ => withdraw k xs
          | NONE => x :: (withdraw k xs)

