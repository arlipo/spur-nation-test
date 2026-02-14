val input =
  """
    |7
    |6 3
    |3 8 5
    |11 2 10 9
    |""".stripMargin

val l = input.split("\n").filterNot(_.isEmpty).toList

case class Acc(nums: List[Int], sum: Int) {
  def isLargerThen(another: Acc): Boolean =
    sum > another.sum

  def add(num: Int): Acc = Acc(nums :+ num, sum + num)
}

l.zipWithIndex.foldLeft(Array.empty[Acc]) { case (ancestors, (inputRow, rowInd)) =>
  val nums = inputRow.split(" ").map(_.toInt)

  val accs = nums.zipWithIndex.flatMap { case (num, numInd) =>
    if (ancestors.isEmpty) {
      Acc(num :: Nil, num) :: Nil
    } else {
      val parents = ancestors.slice(numInd, numInd + 2)
    }
  }


  ???
}

def foo(input: List[String], calculatedAbove: Acc = Acc(Nil, 0), best: Acc = Acc(Nil, 0), x: Int = 0, y: Int = 0): Acc =
  if (input.isEmpty) best
  else {
    val inputRow :: inputTail = input
    val nums                  = inputRow.split(" ").map(_.toInt).toList
    val nextNums              = nums.slice(x, x + 2)
    nextNums.zipWithIndex.foldLeft(best) { case (bestSoFar, (num, index)) =>
      val calculatedCur = Acc(calculatedAbove.nums :+ num, calculatedAbove.sum + num)
      foo(inputTail, calculatedCur, bestSoFar)
    }

    foo(inputTail, calculatedAbove, best)
  }

foo(l)
