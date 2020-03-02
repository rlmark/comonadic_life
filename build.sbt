name := "comonadic_life"

version := "0.1"

scalaVersion := "2.12.9"

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)

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
  "org.typelevel"           %% "cats-effect"              % "2.0.0",
  "co.fs2"                  %% "fs2-core"                 % "2.1.0",
  "com.lihaoyi"             %% "pprint"                   % "0.5.6"
)

mainClass in (Compile, run) := Some("conway.Main")
