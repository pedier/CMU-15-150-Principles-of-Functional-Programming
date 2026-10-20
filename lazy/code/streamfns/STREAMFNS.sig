signature STREAMFNS =
sig
  val concat: 'a Stream.t Stream.t -> 'a Stream.t
  val cycle: 'a Stream.t -> 'a Stream.t
  val interleave: 'a Stream.t -> 'a Stream.t -> 'a Stream.t
  val double: 'a Stream.t -> 'a Stream.t
end
