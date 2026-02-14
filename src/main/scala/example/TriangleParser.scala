package example

import scala.io.Source

object TriangleParser {
  def parseLine(line: String, rowIndex: Int) = {
    val parts = line.trim.split("\\s+").filter(_.nonEmpty)
    val expectedLength = rowIndex + 1
    if (parts.length != expectedLength) {
      throw new IllegalArgumentException(
        s"Row ${rowIndex + 1} should contain $expectedLength numbers but found ${parts.length}"
      )
    }

    parts.map { token =>
      try token.toLong
      catch {
        case _: NumberFormatException =>
          throw new IllegalArgumentException(s"Invalid number '$token' on row ${rowIndex + 1}")
      }
    }.toVector
  }
}
