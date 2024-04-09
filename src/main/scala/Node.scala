class Node(var item: String, var next: Node = null) {
  //  override def toString:String = {
  //    if(next == null){
  //      return item
  //    } else {
  //      return item + " -> " + next.toString
  //    }
  //  }
}

object Lab15_Task3 extends App {
  val list1: List[Node] = List(new Node("Tokyo", new Node("Paris", new Node("Milan"))))
  println(list1)
}