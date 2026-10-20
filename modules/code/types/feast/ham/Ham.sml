signature HAM =
  sig
    type t
    val Boil : t
    val Glaze : t
    val show : t -> string
  end

structure Ham : HAM =
  struct
    datatype t = Boil | Bake | Glaze
    val show = fn Boil => "Boiled Ham"
                | Bake => "Baked Ham"
                | Glaze => "Glazed Ham"
  end

val t1 = Ham.Boil
val x = case t1 of
            Ham.Glaze => Ham.show t1
          | _ => "Not Glazed Ham"
