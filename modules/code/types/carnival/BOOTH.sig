signature BOOTH =
sig
  type t
  type floor = unit
  val addFloor : t -> floor -> t
end