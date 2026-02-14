package example

import cats.data.NonEmptyList
import cats.implicits._
import cats.kernel.Order

final case class MinPathResult(path: Vector[Long], sum: Long)

object MinPathSolver {
  private case class Node(nums: Vector[Long], sum: Long, length: Long) {
    def add(num: Long): Node = Node(nums :+ num, sum + num, length + 1)
  }

  private object Node {
    val default                     = Node(Vector.empty, 0, 0)
    implicit val order: Order[Node] = Order.by(node => (node.length, node.sum))

    def minPreferLongest(node: Node, nodes: Node*): Node =
      nodes.foldLeft(node)((acc, node) =>
        if (acc.length == node.length) Order[Node].min(acc, node)
        else List(acc, node).maxBy(_.length)
      )
  }

  def minimalPath(inputs: List[String]): MinPathResult = {
    require(inputs.nonEmpty, "Triangle must contain at least one row")

    val inputsHead :+ lastRow = inputs
    val ancestors               = inputsHead.zipWithIndex.foldLeft(Array.empty[Node]) { case (ancestors, (inputRow, rowIndex)) =>
      val nums = TriangleParser.parseLine(inputRow, rowIndex)
      nums.zipWithIndex.foldLeft(Array.empty[Node]) { case (rowNodes, (num, numInd)) =>
        val curNode = calculateCurrentNode(ancestors, num, numInd)

        rowNodes :+ curNode
      }
    }

    val nums          = TriangleParser.parseLine(lastRow, inputs.length - 1)
    val (_, bestNode) =
      nums.zipWithIndex.foldLeft((Array.empty[Node], Node.default)) { case ((rowNodes, bestNode), (num, numInd)) =>
        val curNode     = calculateCurrentNode(ancestors, num, numInd)
        val curBestNode = Node.minPreferLongest(curNode, bestNode)

        (rowNodes :+ curNode, curBestNode)
      }

    MinPathResult(bestNode.nums, bestNode.sum)
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
}
