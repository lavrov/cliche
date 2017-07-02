package com.github.lavrov.cliche

class ClicheApp[A](run: ParsedArgs[A] => Unit)(implicit parser: Parser[A]) {
  def main(args: Array[String]): Unit = {
    parser.parse(CommandLineArgs from args).foreach(run)

  }
}
