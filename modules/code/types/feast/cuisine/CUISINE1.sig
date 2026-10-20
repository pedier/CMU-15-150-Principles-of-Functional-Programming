signature CUISINE1  =
sig
  datatype cuisine = Italian | Thai | Chinese
  datatype t = Meal of string * cuisine

  val getCuisine : t -> string
  val rate : t -> (int * cuisine) option
end
