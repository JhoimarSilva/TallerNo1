package object FuncionesRecursivas {

  def maxLin(l: List[Int]): Int = l match {
    case Nil => throw new IllegalArgumentException("La lista no puede ser vacía.")
    case head :: Nil => head
    case head :: tail => Math.max(head, maxLin(tail))

  }

  def maxIt(i: List[Int]): Int = i {

    if(i.isEmpty) println("Debe ingresar un número entero")
    var max = i.head
    for (x <- i.tail){
      if(x> max){
        max = x
      }
    }
    max
  }

}
