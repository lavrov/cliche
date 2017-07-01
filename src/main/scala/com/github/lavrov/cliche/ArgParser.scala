package com.github.lavrov.cliche

trait ArgParser[A] {
  def parse(in: String): Either[String, A]
}

object ArgParser {

  implicit val stringParser: ArgParser[String] = Right(_)

  implicit val boolParser: ArgParser[Boolean] = {
    case "true" => Right(true)
    case "false" => Right(false)
    case _ => Left("not a boolean")
  }

  implicit val intParser: ArgParser[Int] =
    str =>
      if (str matches "\\d+" ) Right(str.toInt)
      else Left("not an integer")

  implicit val charParser: ArgParser[Char] = {
    case string if string.length == 1 => Right(string.head)
    case _ => Left("not a char")
  }
}

trait MultiArgParser[A] {
  def parse(args: List[KeyedArg]): Either[String, A]
}

object MultiArgParser {

  implicit def exactlyOneArg[A](implicit argParser: ArgParser[A]): MultiArgParser[A] = {
    case List(onlyArg) => argParser.parse(onlyArg.value)
    case _ => Left(s"Too many arguments provided")
  }

  implicit def multiArg[A](implicit argParser: ArgParser[A]): MultiArgParser[List[A]] =
    args => {
      def loop(list: List[KeyedArg], result: Either[String, List[A]]): Either[String, List[A]] =
        if (list.isEmpty) result
        else {
          for {
            value <- argParser.parse(list.head.value)
            result <- loop(list.tail, result)
          }
            yield
              value :: result
        }

      loop(args, Right(Nil))
    }

  implicit def optionalMultiArg[A](implicit multiArgParser: MultiArgParser[A]): MultiArgParser[Option[A]] =
    args => multiArgParser.parse(args).map(Some(_))

}
