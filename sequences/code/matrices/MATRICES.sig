signature MATRICES =
sig
  val elementop: ('a * 'a -> 'a) -> 'a Seq.t Seq.t -> 'a Seq.t Seq.t -> 'a Seq.t Seq.t
  val transpose: 'a Seq.t Seq.t -> 'a Seq.t Seq.t
  val multiply: int Seq.t Seq.t -> int Seq.t Seq.t -> int Seq.t Seq.t
end
