lazy val root = (project in file(".")).
  settings(
    name := "tic-tac-toe",
    scalaVersion := "2.11.8",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
  )
