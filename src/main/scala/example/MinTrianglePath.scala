package example

import cats.effect.{IO, IOApp}

import scala.io.Source

object MinTrianglePath extends IOApp.Simple {
  override def run: IO[Unit] =
    (for {
      lines  <- readLines
      result <- MinPathSolver.minimalPath[IO](lines)
      _      <- IO.println(OutputFormatter.format(result))
    } yield ())
      .handleErrorWith(e =>
        IO.blocking {
          Console.err.println(e.getMessage)
          sys.exit(1)
        }
      )

  private def readLines: IO[List[String]] =
    IO.blocking(Source.stdin.getLines().toList)
}
