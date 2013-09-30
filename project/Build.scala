import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  val appName         = "ticTacToe"
  val appVersion      = "1.0-SNAPSHOT"

  val appDependencies = Seq(
    "org.scalatest" % "scalatest_2.10" % "2.0.M5b" % "test",
    "org.mockito" % "mockito-core" % "1.9.5" % "test"
  )


  val main = play.Project(appName, appVersion, appDependencies).settings(
    // Add your own project settings here      
  )

}
