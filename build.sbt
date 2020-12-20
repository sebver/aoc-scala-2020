name := "aoc-scala-2020"

version := "0.1"

scalaVersion := "2.13.4"

libraryDependencies ++= Seq(
  "org.tpolecat"  %% "atto-core" % "0.7.0",
  "com.lihaoyi"   %% "pprint"    % "0.5.6",
  "org.typelevel" %% "cats-core" % "2.1.1",
  "org.scalameta" %% "munit"     % "0.7.20" % Test
)

testFrameworks += new TestFramework("munit.Framework")
