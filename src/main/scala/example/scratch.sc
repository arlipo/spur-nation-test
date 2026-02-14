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

case class Node(nums: List[Int], sum: Int) {
  def add(num: Int): Node = Node(nums :+ num, sum + num)
}

object Node {
  val default                    = Node(Nil, 0)
  implicit val order: Order[Node] = Order.by(acc => acc.sum)
}

def foo(): Unit = {
  val (_, best) = l.foldLeft((Array.empty[Node], Node.default)) { case ((ancestors, best), inputRow) =>
    val nums = inputRow.split(" ").map(_.toInt)

    nums.zipWithIndex.foldLeft((Array.empty[Node], best)) { case ((rowNodes, bestSoFar), (num, numInd)) =>
      val parent1    = ancestors.applyOrElse(numInd, (_: Int) => Node.default)
      val parent2    = ancestors.applyOrElse(numInd + 1, (_: Int) => Node.default)
      val bestParent = Order[Node].min(parent1, parent2)
      val curNode = bestParent.add(num)
      val curBest = Order[Node].min(curNode, bestSoFar)

      (rowNodes :+ curNode, curBest)
    }
  }

  println(best.nums)
}

foo()
