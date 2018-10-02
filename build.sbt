//lazy val scalacheck = "org.scalacheck" %% "scalacheck" % "1.13.4"
//libraryDependencies += scalacheck % Test
//libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test

lazy val projet = (project in file("."))
  .settings(
    name := "Battleship",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test,
  )