import Dependencies._

name := "cliche"

organization := "com.github.lavrov"

version      := "0.1.0-SNAPSHOT"

scalaVersion := "2.12.2"

libraryDependencies ++= Seq(
  shapeless,
  scalaTest % Test
)
