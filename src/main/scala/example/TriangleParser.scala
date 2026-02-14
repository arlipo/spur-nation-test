package example

import cats.MonadThrow
import cats.syntax.all._

object TriangleParser {
  def parseLine[F[_]: MonadThrow](line: String, rowIndex: Int): F[Vector[Long]] = {
    val tokens   = line.trim.split("\\s+").filter(_.nonEmpty).toVector
    val expected = rowIndex + 1

    val parsed =
      if (tokens.length != expected) {
        Left(new IllegalArgumentException(s"Row ${rowIndex + 1} should contain $expected numbers but found ${tokens.length}"))
      } else {
        tokens.traverse { token =>
          Either
            .catchOnly[NumberFormatException](token.toLong)
            .leftMap(_ => new IllegalArgumentException(s"Invalid number '$token' on row ${rowIndex + 1}"))
        }
      }

    parsed.liftTo[F]
  }
}
