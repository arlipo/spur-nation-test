package example

import cats.MonadThrow
import cats.kernel.Order
import cats.syntax.all._

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

  def minimalPath[F[_]: MonadThrow](inputs: List[String]): F[MinPathResult] =
    if (inputs.isEmpty) MonadThrow[F].raiseError(new IllegalArgumentException("Triangle must contain at least one row"))
    else processRows(inputs.zipWithIndex, Array.empty[Node])

  private def processRows[F[_]: MonadThrow](
      rows: List[(String, Int)],
      ancestors: Array[Node]
  ): F[MinPathResult] =
    rows match {
      case (line, idx) :: tail =>
        TriangleParser.parseLine[F](line, idx).flatMap { nums =>
          val rowNodes = nums.zipWithIndex.foldLeft(Array.empty[Node]) { case (rowAcc, (num, numIdx)) =>
            rowAcc :+ calculateCurrentNode(ancestors, num, numIdx)
          }

          if (tail.isEmpty) {
            val bestNode = rowNodes.foldLeft(Node.default) { (bestSoFar, node) =>
              Node.minPreferLongest(node, bestSoFar)
            }
            MinPathResult(bestNode.nums, bestNode.sum).pure[F]
          } else {
            processRows(tail, rowNodes)
          }
        }
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
