package com.github.lavrov.cliche

case class CommandLineArgs(args: List[Arg]) {
  def argsByKey(keys: Set[String]) = {
    val (matched, rest) =
      args.partition {
        case KeyedArg(key, _) if keys contains key => true
        case _ => false
      }
    (matched.collect { case a: KeyedArg => a }, CommandLineArgs(rest))
  }
}

object CommandLineArgs {

  val SingleDash = """-([a-zA-Z])""".r
  val DoubleDash = """--([a-zA-Z]{2,})""".r

  def from(args: Array[String]): CommandLineArgs = {
    def loop(index: Int): List[Arg] = {
      def keyedArg(key: String): List[Arg] =
        args.lift(index + 1).filterNot(_ startsWith "-") match {
          case Some(value) =>
            KeyedArg(key, value) :: loop(index + 2)
          case None =>
            KeyedArg(key, "") :: loop(index + 1)
        }
      args.lift(index) match {
        case Some(string) => string match {
          case SingleDash(key) => keyedArg(key)
          case DoubleDash(key) => keyedArg(key)
          case value => PositionedArg(value) :: loop(index + 1)
        }
        case None =>
          Nil
      }
    }
    CommandLineArgs(loop(0))
  }
}

trait Arg

case class KeyedArg(key: String, value: String) extends Arg

case class PositionedArg(value: String) extends Arg
