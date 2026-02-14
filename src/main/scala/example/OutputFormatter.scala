package example

object OutputFormatter {
  def format(result: MinPathResult): String = {
    val pathExpr = result.path.mkString(" + ")
    s"Minimal path is: $pathExpr = ${result.sum}"
  }
}
