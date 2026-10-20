structure Pie =
  struct
    exception AlreadyEmpty
    type 'a t = 'a list  (* list of slices *)
    val mkPie = fn
      nil => raise AlreadyEmpty
    | pie => pie

    val eat = fn
      nil => NONE
    | _ :: slices => SOME slices
  end
