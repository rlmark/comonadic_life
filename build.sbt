name := "comonadic_life"

version := "0.1"

scalaVersion := "2.13.1"

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "utf-8",
  "-language:existentials",
  "-language:experimental.macros",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-unchecked",
  "-Ywarn-numeric-widen"
)
scalacOptions in(Compile, console) --= Seq("-Ywarn-unused:imports", "-Xfatal-warnings")

libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.0.8",
  "org.typelevel" %% "cats-core" % "2.0.0",
  "org.typelevel" %% "cats-effect" % "2.0.0",
  "co.fs2" %% "fs2-core" % "2.1.0",
  "com.lihaoyi" %% "pprint" % "0.5.6",
  "org.scalatest" %% "scalatest" % "3.0.8" % Test,
  "com.github.alexarchambault" %% "scalacheck-shapeless_1.14" % "1.2.3" % Test,
  "org.typelevel" %% "cats-testkit-scalatest" % "1.0.0-RC1" % Test
)

mainClass in(Compile, run) := Some("conway.Main")
