package object FuncionesRecursivas {

  def maxLin(l: List[Int]): Int = l match {
    case Nil => throw new IllegalArgumentException("La lista no puede ser vacía.")
    case head :: Nil => head
    case head :: tail => Math.max(head, maxLin(tail))

  }

  def maxIt(i: List[Int]): Option[Int] = {
    if (i.isEmpty) {
      None
    } else {
      var max = i.head
      for (n <- i.tail) {
        if (n > max) {
          max = n
        }
      }
      Some(max)
    }
  }

  def movsTorresHanoi(n: Int): Int = {
    if (n == 1) {
      1
    } else {
      (Math.pow(2, n) - 1).toInt
    }
  }

  def torresHanoi(n: Int, origen: Int, auxiliar: Int, destino: Int): List[(Int, Int)] = {
    if (n == 1) List((origen, destino))
    else {
      val movimientos1 = torresHanoi(n - 1, origen, destino, auxiliar)
      val movimientos2 = List((origen, destino))
      val movimientos3 = torresHanoi(n - 1, auxiliar, origen, destino)
      movimientos1 ::: movimientos2 ::: movimientos3
    }
  }
}


