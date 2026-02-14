import cats.data.NonEmptyList
import cats.implicits._
import cats.kernel.Order

val input =
  """
    |7
    |6 3
    |3 8 5
    |11 2 10 9
    |""".stripMargin

val l = input.split("\n").filterNot(_.isEmpty).toList

case class Node(nums: List[Long], sum: Long, length: Long) {
  def add(num: Long): Node = Node(nums :+ num, sum + num, length + 1)
}

object Node {
  val default                    = Node(Nil, 0, 0)
  implicit val order: Order[Node] = Order.by(node => (node.length, node.sum))

  def minPreferLongest(node: Node, nodes: Node*): Node = {
    nodes.foldLeft(node)((acc, node) =>
      if (acc.length == node.length) Order[Node].min(acc, node)
      else List(acc, node).maxBy(_.length)
    )
  }
}

def foo(): Unit = {
  val (_, best) = l.foldLeft((Array.empty[Node], Node.default)) { case ((ancestors, best), inputRow) =>
    val nums = inputRow.split(" ").map(_.toLong)
    nums.zipWithIndex.foldLeft((Array.empty[Node], best)) { case ((rowNodes, bestSoFar), (num, numInd)) =>
      val parent1    = ancestors.applyOrElse(numInd, (_: Int) => Node.default)
      val parent2    = ancestors.applyOrElse(numInd + 1, (_: Int) => Node.default)
      val bestParent = Node.minPreferLongest(parent1, parent2)
      val curNode = bestParent.add(num)
      val curBest = Node.minPreferLongest(curNode, bestSoFar)

      (rowNodes :+ curNode, curBest)
    }
  }

  println(best)
}

foo()
