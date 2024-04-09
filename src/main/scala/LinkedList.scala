class LinkedList(var head: Node = null) {

  private var idx: Int = 0

  def addToStart(s: String): Unit = {
    val newNode: Node = new Node(s, head)
    idx += 1
    head = newNode
  }

  def getSize(): Int = {
    idx
  }

  override def toString: String = {
    def totoString(x: Node): String = {
      if (x == null) {
        return "" + x
      } else {
        return x.item + " -> " + totoString(x.next)
      }
    }

    s"(size ${getSize()}) : " + totoString(head)
  }

  def removeFirstElement(): Unit = {
    if (head == null) {
      return null
    } else {
      var tempNode: Node = head
      head = head.next
      idx -= 1
    }
  }

  def getLastElement(): Node = {
    if (head == null) {
      return null
    }
    var x: Node = head
    while (x.next != null) {
      x = x.next
    }
    x
  }

  def addToEnd(element: String): Unit = {
    if (head == null) {
      addToStart(element)
    } else {
      getLastElement().next = new Node(element)
      idx += 1
    }
  }

  def isPresent(e: String): Boolean = {
    def isisPresent(x: Node): Boolean = {
      if (x == null && e == null) {
        return true
      } else if (x == null && e != null) {
        return false
      } else if (x.item == e) {
        return true
      } else {
        isisPresent(x.next)
      }
    }

    isisPresent(head)
  }

  def findElement(s: String): Node = {
    def findfindElement(x: Node): Node = {
      if (x == null) {
        return null
      } else if (x.item == s) {
        return x
      } else {
        findfindElement(x.next)
      }
    }

    if (!isPresent(s)) {
      return null
    } else {
      findfindElement(head)
    }
  }

  def isBefore(x: String, y: String): Boolean = {
    var x2: Node = findElement(x)
    var y2: Node = findElement(y)
    var xcnt: Int = 0
    var ycnt: Int = 0

    while (x2 != null) {
      xcnt += 1
      x2 = x2.next
    }
    while (y2 != null) {
      ycnt += 1
      y2 = y2.next
    }
    if (xcnt - ycnt > 0) true else false
  }

  // 1,2,3
  // swap(1,3) -> 3,2,1
  // swap(3,1) -> 3,2,1
  def swapElements(e1: String, e2: String): Unit = {
    if (isPresent(e1) && isPresent(e2) && findElement(e1) != null && findElement(e2) != null) {
      if (isBefore(e1, e2)) {
        findElement(e2).item = e1
        findElement(e1).item = e2
      } else {
        findElement(e1).item = e2
        findElement(e2).item = e1
      }
    }
  }

  def removeLastElement(): Unit = {
    if (head == null) {
      return null
    } else if (head.next == null) {
      idx -= 1
      head = null
    } else {
      idx -= 1
      var x: Node = head
      while (x.next.next != null) {
        x = x.next
      }
      x.next = null
    }
  }

  // 1,2,3,4,null
  // 1,3,4,null -> 1.next = 3
  def removeElement(e: String): Unit = {
    if (!isPresent(e)) {
      return
    } else if (head == null) {
      return null
    } else if (head.next == null) {
      idx -= 1
      head = null
    } else if (head.item == e) {
      head = head.next
      idx -= 1
    } else {
      var x: Node = head
      while (x.next.item != e) {
        x = x.next
      }
      x.next = x.next.next
      idx -= 1
    }
  }

  def insertAfter(before: String, after: String): Unit = {
    if (!isPresent(before)) {
      return
    }
    if (before == null) {
      return
    } else {
      var newNode: Node = new Node(after)
      newNode.next = findElement(before).next
      findElement(before).next = newNode
      idx += 1
    }
  }
}


object LinkedList extends App {
  var n: LinkedList = new LinkedList()
  n.addToEnd("Cathy")
  n.addToEnd("Bob")
  n.addToEnd("Alice")
  println(n)
  n.swapElements("Alice", "Bob")
  println(n)
  n.swapElements("Cathy", "Alice")
  println(n)
  //  var flightList: LinkedList = new LinkedList()
  //  //  println(flightList)
  //  flightList.addToStart("Rome")
  //  //  //  println(flightList)
  //  flightList.addToStart("Paris")
  //  //  //  println(flightList)
  //  flightList.addToStart("Tokyo")
  //  //  //  println(flightList)
  //  //  //  println(flightList.getLastElement().item)
  //  flightList.addToEnd("Warsaw")
  //  //  println(flightList)
  //  //  //  flightList.removeFirstElement()
  //  //  //  println(flightList)
  //  //  //  println(flightList.isPresent("Warsawe"))
  //  //  //  println(flightList.findElement("Rome").item)
  //  //  //  flightList.swapElements("Paris", "Rome")
  //  //  //  println(flightList)
  //  //  //  flightList.swapElements("Tokyo","Rome")
  //  //  //  println(flightList)
  //  //  //  flightList.removeElement("Rome")
  //  //  //  println(flightList)
  //  flightList.insertAfter("Paris", "Zurich")
  //  println(flightList)
  //  println(flightList.getSize())
}


