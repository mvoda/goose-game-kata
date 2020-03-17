import sbt._

object Dependencies {
  object Version {
    lazy val scalaTest = "3.1.1"
  }

  lazy val test = Seq(
    "org.scalatest" %% "scalatest" % Version.scalaTest
  )

  lazy val all = test
}
