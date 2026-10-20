signature CUISINE2 =
  sig
    type cuisine
    type t = string * cuisine
    val getCuisine : string * cuisine -> string
  end
