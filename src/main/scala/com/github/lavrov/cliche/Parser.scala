package com.github.lavrov.cliche

import shapeless._
import shapeless.labelled.{FieldType, field}
import shapeless.ops.record.{Keys, Selector}
import shapeless.ops.hlist.ZipWithKeys
import shapeless.record.recordOps

trait Parser[A] {
  def parse(args: CommandLineArgs): Either[String, A]
}

object Parser {

  def apply[A](implicit parser: Parser[A]): Parser[A] = parser

  trait ParserFactory[A, Defaults] {
    def create(defaults: Defaults): Parser[A]
  }


  object ParserFactory {

    implicit def hNilParser[Defaults <: HList]: ParserFactory[HNil, Defaults] =
      _ => {
        case CommandLineArgs(Nil) => Right(HNil)
        case _ => Left("There are not parsed args")
      }

    implicit def hConsParser[K <: Symbol, H, T <: HList, Defaults <: HList](
        implicit
        fieldName: Witness.Aux[K],
        defaultForField: Selector.Aux[Defaults, K, Option[H]],
        defaultForType: Option[TypeDefault[H]] = None,
        multiArgParser: MultiArgParser[H],
        tailFactory: ParserFactory[T, Defaults]
    ): ParserFactory[FieldType[K, H] :: T, Defaults] =
      defaults => {
        args =>
          val (matchedArgs, restArgs) = args.argsByKey(Set(fieldName.value.name))
          val defaultValue = defaultForField(defaults) orElse defaultForType.map(_.value)
          val eitherValue =
            if (matchedArgs.isEmpty)
              defaultValue.toRight(s"Missing argument ${fieldName.value.name}")
            else
              multiArgParser.parse(matchedArgs)
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
}
