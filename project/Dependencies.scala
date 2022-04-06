import sbt._

object Versions {
  val munit = "1.0.0-M3"
}

object Dependencies {
  lazy val munit = "org.scalameta" %% "munit" % Versions.munit
}
