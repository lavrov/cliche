package com.github.lavrov.cliche

class ClicheApp[A](run: ParsedArgs[A] => Unit)(implicit parserAndHelp: ParserAndHelp[A]) {
  def main(args: Array[String]): Unit = {
    val cliArgs = CommandLineArgs from args
    val helpParser = implicitly[ParserAndHelp[HelpArgs]]
    helpParser.parser.parse(cliArgs).fold(
      _ => parserAndHelp.parser.parse(cliArgs).fold(println, run),
      _ => {
        val completeHelp = helpParser.help.lines ::: parserAndHelp.help.lines
        println(completeHelp mkString "\n")
      }
    )
  }
}
