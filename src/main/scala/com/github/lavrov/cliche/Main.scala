package com.github.lavrov.cliche

case class Parameters(bufferSize: Option[Int] = Some(1024), sequentially: Boolean = false)
case class Copy(@Recurse parameters: Parameters, from: String, to: List[String])

sealed trait Command

case class Start() extends Command

case class Stop() extends Command

object Main extends ClicheApp[Command](
  result =>
    println(result)
)
