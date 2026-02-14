package example

import cats.effect.IO
import munit.CatsEffectSuite

class MinPathSolverSpec extends CatsEffectSuite {
  test("computes minimal path and sum for small triangle") {
    val lines = List(
      "7",
      "6 3",
      "3 8 5",
      "11 2 10 9"
    )

    MinPathSolver
      .minimalPath[IO](lines)
      .map { result =>
        assertEquals(result.path, Vector(7L, 6L, 3L, 2L))
        assertEquals(result.sum, 18L)
      }
  }

  test("fails when input is empty") {
    interceptMessageIO[IllegalArgumentException]("Triangle must contain at least one row") {
      MinPathSolver.minimalPath[IO](Nil).void
    }
  }

  test("fails when a row has wrong length") {
    val malformed = List(
      "7",
      "6",
      "3 8 5"
    )

    interceptMessageIO[IllegalArgumentException]("Row 2 should contain 2 numbers but found 1") {
      MinPathSolver.minimalPath[IO](malformed).void
    }
  }

  test("fails when a token is not a number") {
    val malformed = List(
      "7",
      "6 two",
      "3 8 5"
    )

    interceptMessageIO[IllegalArgumentException]("Invalid number 'two' on row 2") {
      MinPathSolver.minimalPath[IO](malformed).void
    }
  }
}
