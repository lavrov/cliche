package com.github.lavrov.cliche

import shapeless.{:: => #:, _}
import ops.hlist.IsHCons
import shapeless.labelled.{FieldType, field}

import scala.reflect.ClassTag

trait Parser[A] {
  def parse(args: CommandLineArgs): Either[String, ParsedArgs[A]]
}

case class ParsedArgs[A](value: A, unparsed: CommandLineArgs = CommandLineArgs(Nil))

case class ParserAndHelp[A](parser: Parser[A], help: Help)

object ParserAndHelp {

  def apply[A](implicit parser: Parser[A]): Parser[A] = parser

  trait ParserFactory[A, Defaults, Recurse] {
    def create(defaults: Defaults, recurse: Recurse): ParserAndHelp[A]
  }

  object ParserFactory {

    implicit val hNilParser: ParserFactory[HNil, HNil, HNil] =
      (_, _) => ParserAndHelp(
        args => Right(ParsedArgs(HNil, args)),
        Help(Nil)
      )

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
        val fieldName = fieldNameWitness.value.name
        val argKeys = Set(fieldName, camelcaseToDashSeparated(fieldName))
        val tailParser = tailFactory.create(defaultForField tail defaults, recurseForField tail recurse)
        ParserAndHelp(
          args => {
            val (matchedArgs, restArgs) = args.argsByKey(argKeys)
            val defaultValue = defaultForField.head(defaults) orElse defaultForType.map(_.value)
            val eitherValue =
              if (matchedArgs.isEmpty)
                defaultValue.toRight(s"Missing argument $fieldName")
              else
                multiArgParser.parse(matchedArgs)
            eitherValue.flatMap { value =>
              tailParser.parser.parse(restArgs).map {
                case ParsedArgs(tailResult, unparsed) =>
                  ParsedArgs(field[K](value) :: tailResult, unparsed)
              }
            }
          },
          Help(
            argKeys.toList.map { key => if (key.length > 1) s"--$key" else s"-$key"}.mkString(" ") ::
            tailParser.help.lines)
        )
    }

    implicit def hListRecursiveParser[K <: Symbol, H, T <: HList, DH, Defaults <: HList, DT <: HList, Recurses <: HList, RT <: HList](
        implicit
        defaultForField: IsHCons.Aux[Defaults, Option[H], DT],
        recurseForField: IsHCons.Aux[Recurses, Some[Recurse], RT],
        parser: ParserAndHelp[H],
        tailFactory: ParserFactory[T, DT, RT]
    ): ParserFactory[FieldType[K, H] #: T, Defaults, Recurses] = {
      (defaults, recurse) =>
        val tailParser = tailFactory.create(defaultForField tail defaults, recurseForField tail recurse)
        ParserAndHelp(
        args =>
          parser.parser.parse(args).flatMap {
            case ParsedArgs(value, unparsed) =>
              tailParser.parser.parse(unparsed).map {
                case ParsedArgs(tailResult, unparsedTail) =>
                  ParsedArgs(field[K](value) :: tailResult, unparsedTail)
              }
          },
        Help(parser.help.lines ::: tailParser.help.lines)
      )
    }
  }

  def camelcaseToDashSeparated(in: String): String =
    """.([A-Z]).""".r.replaceAllIn(in, m => s"-${m group 1}").toLowerCase

  implicit def genericParser[A, Rep <: HList, K <: HList, Defaults <: HList, Recurses <: HList](
      implicit
      generic: LabelledGeneric.Aux[A, Rep],
      defaults: Default.AsOptions.Aux[A, Defaults],
      recurses: Annotations.Aux[Recurse, A, Recurses],
      parserFactory: ParserFactory[Rep, Defaults, Recurses]
  ): ParserAndHelp[A] = {
    val parserAndHelp = parserFactory.create(defaults(), recurses())
    ParserAndHelp(
      args =>
        parserAndHelp.parser.parse(args).map {
          case ParsedArgs(genRep, unparsed) =>
            ParsedArgs(generic from genRep, unparsed)
        },
      parserAndHelp.help
    )
  }

  implicit val cNilParser: ParserAndHelp[CNil] =
    ParserAndHelp(_ => Left("Command not found"), Help(Nil))

  implicit def coproductParser[H, T <: Coproduct](
      implicit
      hParser: ParserAndHelp[H],
      tParser: ParserAndHelp[T],
      classTag: ClassTag[H]
  ): ParserAndHelp[H :+: T] = {
    val commandKey = camelcaseToDashSeparated(classTag.runtimeClass.getSimpleName)
    ParserAndHelp(
      {
        case args@CommandLineArgs(PositionedArg(commandName) :: rest) =>
          if (commandName == commandKey)
            hParser.parser.parse(CommandLineArgs(rest)).map {
              case ParsedArgs(value, unparsed) =>
                ParsedArgs(Inl(value), unparsed)
            }
          else
            tParser.parser.parse(args).map {
              case ParsedArgs(value, unparsed) =>
                ParsedArgs(Inr(value), unparsed)
            }
        case _ => Left("There must be a command name first")
      },
      Help(commandKey :: hParser.help.lines.map("  " + _) ::: tParser.help.lines)
    )
  }

  implicit def genericCoproductParser[A, Rep <: Coproduct](
      implicit
      generic: Generic.Aux[A, Rep],
      parser: ParserAndHelp[Rep]
  ): ParserAndHelp[A] =
    ParserAndHelp(
      args =>
        parser.parser.parse(args).map {
          case ParsedArgs(value, unparsed) =>
            ParsedArgs(generic from value, unparsed)
        },
      Help("Supported commands: " :: parser.help.lines.map("  " + _))
    )
}
