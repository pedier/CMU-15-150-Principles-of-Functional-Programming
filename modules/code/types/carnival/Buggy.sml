structure Buggy =
struct
  datatype t = Apex | Atlas | CIA | DG | Fringe | PiKA
             | Robobuggy | SAE | SDC | SigEp | SigNu | Spirit

  val winner2024 = CIA

  val r = Random.rand (12,34)
  fun race buggy1 buggy2 = if Random.randReal r > 0.5 then buggy1 else buggy2
end