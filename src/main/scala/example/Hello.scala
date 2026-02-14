import cats.data.NonEmptyList
import cats.implicits._
import cats.kernel.Order

object App extends App {

  val input =
    """
      |7
      |6 3
      |3 8 5
      |11 2 10 9
      |""".stripMargin

  val inputs = input.split("\n").filterNot(_.isEmpty).toList

  case class Node(nums: List[Long], sum: Long, length: Long) {
    def add(num: Long): Node = Node(nums :+ num, sum + num, length + 1)
  }

  object Node {
    val default                     = Node(Nil, 0, 0)
    implicit val order: Order[Node] = Order.by(node => (node.length, node.sum))

    def minPreferLongest(node: Node, nodes: Node*): Node =
      nodes.foldLeft(node)((acc, node) =>
        if (acc.length == node.length) Order[Node].min(acc, node)
        else List(acc, node).maxBy(_.length)
      )
  }

  def foo(): Unit = {
    val (inputsHead :+ lastRow) = inputs
    val ancestors               = inputsHead.foldLeft(Array.empty[Node]) { case (ancestors, inputRow) =>
      val nums = inputRow.split(" ").map(_.toLong)
      nums.zipWithIndex.foldLeft(Array.empty[Node]) { case (rowNodes, (num, numInd)) =>
        val curNode = calculateCurrentNode(ancestors, num, numInd)

        rowNodes :+ curNode
      }
    }

    val nums          = lastRow.split(" ").map(_.toLong)
    val (_, bestNode) =
      nums.zipWithIndex.foldLeft((Array.empty[Node], Node.default)) { case ((rowNodes, bestNode), (num, numInd)) =>
        val curNode     = calculateCurrentNode(ancestors, num, numInd)
        val curBestNode = Node.minPreferLongest(curNode, bestNode)

        (rowNodes :+ curNode, curBestNode)
      }

    println(bestNode)
  }

  private def calculateCurrentNode(ancestors: Array[Node], num: Long, numInd: Int) = {
    val bestParent =
      if (numInd == 0) ancestors.applyOrElse(0, (_: Int) => Node.default)
      else {
        val parent1 = ancestors.applyOrElse(numInd - 1, (_: Int) => Node.default)
        val parent2 = ancestors.applyOrElse(numInd, (_: Int) => Node.default)
        Node.minPreferLongest(parent1, parent2)
      }

    bestParent.add(num)
  }

  foo()

}
