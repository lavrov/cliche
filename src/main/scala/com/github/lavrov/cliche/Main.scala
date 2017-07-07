package com.github.lavrov.cliche

case class Parameters(bufferSize: Option[Int] = Some(1024), sequentially: Boolean = false)
case class Copy(@Recurse parameters: Parameters, from: String, to: List[String])

sealed trait Command

case class StartProcess(delay: Int) extends Command

case class Stop() extends Command

object Main extends ClicheApp[Command](
  result =>
    println(result)
)
