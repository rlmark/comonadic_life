name := "comonadic_life"

version := "0.1"

scalaVersion := "2.12.9"

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "utf-8",
  "-language:existentials",
  "-language:experimental.macros",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-unchecked",
  "-Ypartial-unification",
  "-Ywarn-numeric-widen"
)
scalacOptions in (Compile, console) --= Seq("-Ywarn-unused:imports", "-Xfatal-warnings")

libraryDependencies ++= Seq(
  "org.scalatest"           %% "scalatest"                % "3.0.8" % Test,
  "org.scalactic"           %% "scalactic"                % "3.0.8",
  "com.lihaoyi" %% "pprint" % "0.5.6"
)
