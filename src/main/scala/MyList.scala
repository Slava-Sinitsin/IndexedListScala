import java.io._
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks.{break, breakable}

class MyList[T1](n: Int) {
  class MyNode[T2](data: T2) {
    var prev: MyNode[T2] = _
    var next: MyNode[T2] = _
    var value: T2 = data
  }

  private var head: MyNode[T1] = _
  private var tail: MyNode[T1] = _
  private var size: Int = 0
  private val vector: ArrayBuffer[MyNode[T1]] = ArrayBuffer.empty

  def getN: Int = n

  //noinspection ScalaUnusedSymbol
  def getSize: Int = size

  def add(data: T1): Unit = {
    val newNode = new MyNode(data)
    if (head == null) {
      head = newNode
    } else {
      tail.next = newNode
      newNode.prev = tail
    }
    tail = newNode
    size += 1
    if (size % n == 0) {
      vector += newNode
    }
  }

  //noinspection ScalaUnusedSymbol
  def get(index: Int): T1 = {
    if (index < 0 || index >= size) {
      throw new IndexOutOfBoundsException("Index is out of bounds.")
    }
    var current = head
    for (_ <- 0 until index) {
      current = current.next
    }
    current.value
  }

  def remove(index: Int): Unit = {
    if (index < 0 || index >= size) {
      throw new IndexOutOfBoundsException("Index is out of bounds.")
    }
    if (index == 0) {
      head = head.next
      if (head != null) {
        head.prev = null
      }
      if (size == 1) {
        tail = null
      }
    } else {
      var current: MyNode[T1] = null
      if (vector.size > 1 && index >= n) {
        current = vector(index / n - 1)
        for (_ <- (vector.size * n - index) until (index - 1)) {
          current = current.next
        }
      } else {
        current = head
        for (_ <- 0 until (index - 1)) {
          current = current.next
        }
      }
      current.next = current.next.next
      if (current.next != null) {
        current.next.prev = current
      }
      if (current.next == null) {
        tail = current
      }
    }
    size -= 1
    updateVector(index, "rem")
  }

  def insert(index: Int, data: T1): Unit = {
    val newNode = new MyNode(data)
    if (index < 0 || index > size) {
      throw new IndexOutOfBoundsException("Index is out of bounds.")
    }
    if (index == 0) {
      newNode.next = head
      if (head != null) {
        head.prev = newNode
      }
      head = newNode
      if (size == 0) {
        tail = newNode
      }
    } else {
      var current: MyNode[T1] = null
      if (vector.size > 1 && index >= n) {
        current = vector(index / n - 1)
        for (_ <- (n * (index / n)) until index) {
          current = current.next
        }
      } else {
        current = head
        for (_ <- 0 until (index - 1)) {
          current = current.next
        }
      }
      newNode.next = current.next
      if (current.next != null) {
        current.next.prev = newNode
      }
      current.next = newNode
      newNode.prev = current
      if (newNode.next == null) {
        tail = newNode
      }
    }
    size += 1
    updateVector(index, "add")
  }

  private def updateVector(index: Int, op: String): Unit = {
    if (op == "add") {
      for (i <- (index / n) until vector.size) {
        vector(i) = vector(i).prev
      }
      if (size % n == 0) {
        vector += tail
      }
    }
    if (op == "rem") {
      breakable {
        for (i <- (index / n) until vector.size) {
          if (vector(i).next == null) {
            vector.remove(i)
            break
          } else {
            vector(i) = vector(i).next
          }
        }
      }
    }
    if (op == "sort") {
      vector.clear()
      var current = head
      var count = 1
      while (current != null) {
        if (count % n == 0) {
          vector += current
        }
        current = current.next
        count += 1
      }
      current = head
      current.prev = null
      while (current.next != null) {
        current.next.prev = current
        current = current.next
      }
      tail = current
    }
  }

  //noinspection ScalaUnusedSymbol
  def display(): Unit = {
    var current = head
    print("null <- ")
    while (current != null) {
      print(current.value)
      if (current.next != null) {
        print(" <-> ")
      } else {
        print(" -> ")
      }
      current = current.next
    }
    println("null")
    print(s"N = $n: ")
    for (node <- vector) {
      print(s"[${node.value}] ")
    }
    println()
  }

  override def toString: String = {
    val builder = new StringBuilder()
    var current = head
    builder.append("null <- ")
    while (current != null) {
      builder.append(current.value)
      if (current.next != null) {
        builder.append(" <-> ")
      } else {
        builder.append(" -> ")
      }
      current = current.next
    }
    builder.append("null")
    builder.toString()
  }

  def vectorToString(): String = {
    val builder = new StringBuilder()
    builder.append(s"N = $n: ")
    for (node <- vector) {
      builder.append(s"[${node.value}] ")
    }
    builder.toString()
  }

  def mergeSort(): Unit = {
    head = mergeSort(head)
    tail = findTail(head)
    updateVector(0, "sort")
  }

  private def mergeSort(head: MyNode[T1]): MyNode[T1] = {
    if (head == null || head.next == null) {
      return head
    }

    val middle = findMiddle(head)
    val secondHalf = middle.next
    middle.next = null

    val left = mergeSort(head)
    val right = mergeSort(secondHalf)

    merge(left, right)
  }

  private def findMiddle(head: MyNode[T1]): MyNode[T1] = {
    if (head == null) {
      return null
    }

    var slow = head
    var fast = head

    while (fast.next != null && fast.next.next != null) {
      slow = slow.next
      fast = fast.next.next
    }

    slow
  }

  private def merge(left: MyNode[T1], right: MyNode[T1]): MyNode[T1] = {
    var result: MyNode[T1] = null
    if (left == null) {
      return right
    }
    if (right == null) {
      return left
    }
    if (compare(left.value, right.value) <= 0) {
      result = left
      result.next = merge(left.next, right)
    } else {
      result = right
      result.next = merge(left, right.next)
    }
    result
  }

  private def findTail(head: MyNode[T1]): MyNode[T1] = {
    if (head == null) {
      return null
    }

    var current = head
    while (current.next != null) {
      current = current.next
    }

    current
  }

  private def compare(a: T1, b: T1): Int = {
    a match {
      case value: Comparable[T1] if b.isInstanceOf[Comparable[T1]] =>
        value.compareTo(b)
      case _ =>
        throw new IllegalArgumentException("Elements must implement Comparable interface.")
    }
  }

  trait Callback[T] {
    def toDo(v: T): T
  }

  def forEach(callback: Callback[T1]): Unit = {
    var current = head
    while (current != null) {
      current.value = callback.toDo(current.value)
      current = current.next
    }
  }

  def serializeToFile(filename: String): Unit = {
    try {
      val writer = new BufferedWriter(new FileWriter(filename))
      val json = new StringBuilder()
      json.append("{")
      json.append("\"n\":").append(n).append(",")
      json.append("\"data\":[")
      var current = head
      while (current != null) {
        current.value match {
          case point: Point2D =>
            json.append("\"(").append(point.getX).append(";").append(point.getY).append(")\",")
          case _ =>
            json.append("\"").append(current.value.toString).append("\",")
        }
        current = current.next
      }
      if (json.charAt(json.length - 1) == ',') {
        json.setCharAt(json.length - 1, ']')
      } else {
        json.append("]")
      }
      json.append("}")
      writer.write(json.toString())
      writer.close()
    } catch {
      case e: IOException =>
        e.printStackTrace()
    }
  }
}

object FileReader {
  def deserializeFromFile[E](filename: String, elementType: Class[E]): MyList[E] = {
    var myList: MyList[E] = null
    try {
      val reader = new BufferedReader(new FileReader(filename))
      val json = new StringBuilder()
      var line: String = null
      while ({line = reader.readLine(); line != null}) {
        json.append(line)
      }

      val jsonString = json.toString()
      val index = jsonString.indexOf("\"n\":") + 4
      val endIndex = jsonString.indexOf(",", index)
      val n = Integer.parseInt(jsonString.substring(index, endIndex))
      myList = new MyList[E](n)

      val dataStartIndex = jsonString.indexOf("\"data\":[") + 8
      val dataEndIndex = jsonString.lastIndexOf("]")
      val dataString = jsonString.substring(dataStartIndex, dataEndIndex)
      val elements = dataString.split(",")
      for (element <- elements) {
        if (elementType == classOf[Integer]) {
          val value = Integer.parseInt(element.substring(1, element.length - 1))
          myList.add(value.asInstanceOf[E])
        } else if (elementType == classOf[Point2D]) {
          val coords = element.substring(2, element.length - 2).split(";")
          val x = coords(0).trim.toDouble
          val y = coords(1).trim.toDouble
          val point = new Point2D(x, y)
          myList.add(point.asInstanceOf[E])
        }
      }
    } catch {
      case e: IOException => e.printStackTrace()
    }
    myList
  }
}