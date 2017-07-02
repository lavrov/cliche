package com.github.lavrov.cliche

case class Parameters(bufferSize: Option[Int] = Some(1024), sequentially: Boolean = false)
case class Copy(@Recurse parameters: Parameters, from: String, to: List[String])

object Main extends ClicheApp[Copy](
  result =>
    println(result)
)
