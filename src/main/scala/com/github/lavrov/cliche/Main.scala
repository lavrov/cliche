package com.github.lavrov.cliche


object Main extends App {

  case class Copy(from: String, to: List[String], bufferSize: Option[Int] = Some(1024), sequentially: Boolean = false)

  val commandLineArgs = CommandLineArgs.from(args)

  println(commandLineArgs)

  println {
    Parser[Copy].parse(commandLineArgs)
  }
}
