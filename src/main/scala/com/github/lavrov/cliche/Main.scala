package com.github.lavrov.cliche


object Main extends App {

  case class Parameters(bufferSize: Option[Int] = Some(1024), sequentially: Boolean = false)
  case class Copy(@Recurse parameters: Parameters, from: String, to: List[String])

  val commandLineArgs = CommandLineArgs.from(args)

  println(commandLineArgs)

  println {
    Parser[Copy].parse(commandLineArgs)
  }
}
