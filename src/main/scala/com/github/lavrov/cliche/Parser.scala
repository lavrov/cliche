package com.github.lavrov.cliche

import shapeless.{:: => #:, _}
import ops.hlist.IsHCons
import shapeless.labelled.{FieldType, field}

import scala.reflect.ClassTag

trait Parser[A] {
  def parse(args: CommandLineArgs): Either[String, ParsedArgs[A]]
}

case class ParsedArgs[A](value: A, unparsed: CommandLineArgs = CommandLineArgs(Nil))

object Parser {

  def apply[A](implicit parser: Parser[A]): Parser[A] = parser

  trait ParserFactory[A, Defaults, Recurse] {
    def create(defaults: Defaults, recurse: Recurse): Parser[A]
  }


  object ParserFactory {

    implicit val hNilParser: ParserFactory[HNil, HNil, HNil] =
      (_, _) =>
        args => Right(ParsedArgs(HNil, args))

    implicit def hListParser[K <: Symbol, H, T <: HList, DH, Defaults <: HList, DT <: HList, Recurses <: HList, RT <: HList](
        implicit
        fieldNameWitness: Witness.Aux[K],
        defaultForField: IsHCons.Aux[Defaults, Option[H], DT],
        defaultForType: Option[TypeDefault[H]] = None,
        recurseForField: IsHCons.Aux[Recurses, None.type, RT],
        multiArgParser: MultiArgParser[H],
        tailFactory: ParserFactory[T, DT, RT]
    ): ParserFactory[FieldType[K, H] #: T, Defaults, Recurses] = {
      (defaults, recurse) =>
        args =>
          val fieldName = fieldNameWitness.value.name
          val argKeys = Set(fieldName, camelcaseToDashSeparated(fieldName))
          val (matchedArgs, restArgs) = args.argsByKey(argKeys)
          val defaultValue = defaultForField.head(defaults) orElse defaultForType.map(_.value)
          val eitherValue =
            if (matchedArgs.isEmpty)
              defaultValue.toRight(s"Missing argument $fieldName")
            else
              multiArgParser.parse(matchedArgs)
          eitherValue.flatMap { value =>
             tailFactory.create(defaultForField tail defaults, recurseForField tail recurse).parse(restArgs).map {
               case ParsedArgs(tailResult, unparsed) =>
                 ParsedArgs(field[K](value) :: tailResult, unparsed)
             }
          }
      }

    implicit def hListRecursiveParser[K <: Symbol, H, T <: HList, DH, Defaults <: HList, DT <: HList, Recurses <: HList, RT <: HList](
        implicit
        defaultForField: IsHCons.Aux[Defaults, Option[H], DT],
        recurseForField: IsHCons.Aux[Recurses, Some[Recurse], RT],
        parser: Parser[H],
        tailFactory: ParserFactory[T, DT, RT]
    ): ParserFactory[FieldType[K, H] #: T, Defaults, Recurses] = {
      (defaults, recurse) =>
        args =>
          parser.parse(args).flatMap {
            case ParsedArgs(value, unparsed) =>
              tailFactory.create(defaultForField tail defaults, recurseForField tail recurse).parse(unparsed).map {
                case ParsedArgs(tailResult, unparsedTail) =>
                  ParsedArgs(field[K](value) :: tailResult, unparsedTail)
              }
          }
    }

    def camelcaseToDashSeparated(in: String): String = """[A-Z]""".r.replaceAllIn(in, m => s"-${m.matched.toLowerCase}")
  }

  implicit def genericParser[A, Rep <: HList, K <: HList, Defaults <: HList, Recurses <: HList](
      implicit
      generic: LabelledGeneric.Aux[A, Rep],
      defaults: Default.AsOptions.Aux[A, Defaults],
      recurses: Annotations.Aux[Recurse, A, Recurses],
      parserFactory: ParserFactory[Rep, Defaults, Recurses]
  ): Parser[A] = {
    args =>
      parserFactory.create(defaults(), recurses()).parse(args).map {
        case  ParsedArgs(genRep, unparsed) =>
          ParsedArgs(generic from genRep, unparsed)
      }
  }

  implicit val cNilParser: Parser[CNil] = _ => Left("Command not found")

  implicit def coproductParser[H, T <: Coproduct](
      implicit
      hParser: Parser[H],
      tParser: Parser[T],
      classTag: ClassTag[H]
  ): Parser[H :+: T] = {
    case args@CommandLineArgs(PositionedArg(commandName) :: rest) =>
      if (commandName == classTag.runtimeClass.getSimpleName)
        hParser.parse(CommandLineArgs(rest)).map {
          case ParsedArgs(value, unparsed) =>
            ParsedArgs(Inl(value), unparsed)
        }
      else
        tParser.parse(args).map {
          case ParsedArgs(value, unparsed) =>
            ParsedArgs(Inr(value), unparsed)
        }
    case _ => Left("There must be a command name first")
  }

  implicit def genericCoproductParser[A, Rep <: Coproduct](
      implicit
      generic: Generic.Aux[A, Rep],
      parser: Parser[Rep]
  ): Parser[A] = {
    args =>
      parser.parse(args).map {
        case ParsedArgs(value, unparsed) =>
          ParsedArgs(generic from value, unparsed)
      }
  }
}
