def f(x) =
  val y =
    val z =
      if x=0 then 1 else f(x-1) end
    in z+1
    end
  in y+2
  end
eval f(3) /* resultat: 13 */