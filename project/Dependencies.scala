import sbt._

object Dependencies {
  lazy val munit = "org.scalameta" %% "munit" % "0.7.29"
  lazy val catsEffect = "org.typelevel" %% "cats-effect" % "3.5.4"
  lazy val munitCatsEffect = "org.typelevel" %% "munit-cats-effect-3" % "1.0.7"
}
