

object Dijkstra {
 
  type Path[Key] = (Double, List[Key])
 
  def Dijkstra[Key](graph: Map[Key, List[(Double, Key)]], fringe: List[Path[Key]], dest: Key, passed: Set[Key]): Path[Key] = fringe 
  match {
    case (distance, path) :: fringe_rest => path 
    match {
      case key :: path_rest =>
      if (key == dest) (distance, path.reverse)
      else {
        val paths = graph(key).flatMap 
        {
          case (d, key) =>
            if (!passed.contains(key)) List((distance + d, key :: path)) 
            else Nil
        }
        val sorted_fringe = (paths ++ fringe_rest).sortWith 
        {
          case ((d1, _), (d2, _)) => 
            d1 < d2
        }
        Dijkstra(graph, sorted_fringe, dest, passed + key)
      }
    }
    case Nil => (0, List())
  }
 
  def main(x: Array[String]): Unit = {
    val graph1 = Map(
      "a" -> List((2.0, "b"), (2.0, "e"), (2.0, "h"), (2.0, "k")),
      "b" -> List((4.0, "c"), (1.0, "h"), (2.0, "i")),
      "c" -> List((3.0, "d"), (2.0, "i"), (4.0, "j")),
      "d" -> List((2.0, "j"), (1.0, "n")),
      "e" -> List((3.0, "f"), (1.0, "k")),
	    "f" -> List((4.0, "g"), (2.0, "k"), (1.0, "l"), (3.0, "m")),
      "g" -> List((1.0, "m"), (1.0, "n")),
      "h" -> List((4.0, "i"), (2.0, "k")),
      "i" -> List((5.0, "j"), (3.0, "k"), (1.0, "l")),
      "j" -> List((2.0, "l"), (2.0, "m"), (1.0, "n")),
	    "k" -> List((4.0, "l")),
      "l" -> List((3.0, "m")),
      "m" -> List((3.0, "n")),
      "n" ->  Nil
    )
	val graph2 = Map(
      "a" -> List((2.0, "b"), (3.0, "c")),
      "b" -> List((3.0, "c"), (2.0, "d"), (2.0, "e")),
      "c" -> List((2.0, "e")),
      "d" -> List((1.0, "e"), (4.0, "f")),
      "e" -> List((3.0, "f")),
	    "f" -> Nil
    )
    var s = System.nanoTime
    var res = Dijkstra[String](graph1, List((0, List("a"))), "m", Set())
    println("time: "+(System.nanoTime-s)/1e6+"ms")
    println(res)
    s = System.nanoTime
    res = Dijkstra[String](graph2, List((0, List("a"))), "e", Set())
    println("time: "+(System.nanoTime-s)/1e6+"ms")
    println(res)
  }
}