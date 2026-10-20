structure Cuisine =
  struct
    datatype cuisine = Italian | Thai | Chinese
    type t = string * cuisine          (* restaurant, cuisine *)

    val getCuisine = fn
      (_, Italian) => "Italian"
    | (_, Thai) => "Thai"
    | (_, Chinese) => "Chinese"

    val rate = fn
      ("Rose Tea Cafe", k) => SOME (10, k)
    | (_, _) => NONE
  end
