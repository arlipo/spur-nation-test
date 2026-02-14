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
}
