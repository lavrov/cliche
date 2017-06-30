package com.github.lavrov.cliche

import shapeless._
import shapeless.labelled.{FieldType, field}
import shapeless.ops.record.{Keys, Selector}
import shapeless.ops.hlist.ZipWithKeys
import shapeless.record.recordOps

object Main extends  App {

  implicit val boolParser: ArgParser[Boolean] = {
    case "true" => Right(true)
    case "false" => Right(false)
    case _ => Left("not a boolean")
  }

  implicit val intParser: ArgParser[Int] =
    str => if (str.matches("\\d+")) Right(str.toInt) else Left("not an integer")

  trait ParserFactory[A, Defaults] {
    def create(defaults: Defaults): Parser[A]
  }

  object ParserFactory {

    implicit def hNilParser[Defaults <: HList]: ParserFactory[HNil, Defaults] =
      _ => {
        case Nil => Right(HNil)
        case _ => Left("There are not parsed args")
      }

    implicit def hConsParser[K <: Symbol, H, T <: HList, Defaults <: HList](
        implicit
        name: Witness.Aux[K],
        defaultSelector: Selector.Aux[Defaults, K, Option[H]],
        argParser: ArgParser[H],
        tailFactory: ParserFactory[T, Defaults]
    ): ParserFactory[FieldType[K, H] :: T, Defaults] =
      defaults => {
        args =>
          val (eitherValue, restArgs) =
            args.find(_.key == name.value.name) match {
              case None =>
                (defaults.get(name).toRight(s"Missing ${name.value.name} option"), args)
              case Some(arg@Arg(key, string)) =>
                (argParser parse string, args.filterNot(_ == arg))
            }
          for {
            tailResult <- tailFactory.create(defaults).parse(restArgs)
            value <- eitherValue
          }
          yield
            field[K](value) :: tailResult
      }
  }

  implicit def genericParser[A, Rep <: HList, K <: HList, DefaultsOption <: HList, Defaults <: HList, Zipped <: HList, P](
      implicit
        generic: LabelledGeneric.Aux[A, Rep],
        keys: Keys.Aux[Rep, K],
        defaultsAsOption: Default.AsOptions.Aux[A, DefaultsOption],
        toRecord: ZipWithKeys.Aux[K, DefaultsOption, Defaults],
        parserFactory: ParserFactory[Rep, Defaults]
  ): Parser[A] = {
    args =>
      parserFactory.create(toRecord(defaultsAsOption())).parse(args).map(generic.from)
  }

  case class Cmd(n: Int = 1, b: Boolean = false)

  println {
    implicitly[Parser[Cmd]].parse(Arg("b", "true") :: Nil)
  }
}

case class Arg(key: String, value: String)

trait Parser[A] {
  def parse(args: List[Arg]): Either[String, A]
}

trait ArgParser[A] {
  def parse(in: String): Either[String, A]
}

case class DefaultValue[A](value: A)

object DefaultValue {
  implicit def optionDefault[T] = DefaultValue[Option[T]](None)
}
