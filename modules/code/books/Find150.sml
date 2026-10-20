structure Find150 :> sig val is150Book : Book.page list -> bool end =
struct

  (* is150Book : Book.page list -> bool
   * REQUIRES: true
   * ENSURES:
   *   is150Book b = true iff b contains at least three instance of
   *   the word "lambda" and no instances of the word "pointer"
   *)
  structure NoPtrHelper = MkNemesis (Honk)
  structure ThreeLam = MkCompose (structure StudentA = Reset
                                  structure StudentB = Polly)
  structure NoPtr = MkCompose (structure StudentA = Reset
                               structure StudentB = NoPtrHelper)
  structure Sat = MkCompose (structure StudentA = ThreeLam
                             structure StudentB = NoPtr)

  structure Reader = MkReader (Sat)
  val is150Book = Reader.read o Book.bind
end
