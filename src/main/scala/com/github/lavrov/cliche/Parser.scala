package com.github.lavrov.cliche

import shapeless._
import shapeless.labelled.{FieldType, field}

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

    implicit def hConsParser[K <: Symbol, H, T <: HList, DH, Defaults <: HList, DT <: HList](
        implicit
        fieldName: Witness.Aux[K],
        defaultForField: shapeless.ops.hlist.IsHCons.Aux[Defaults, Option[H], DT],
        defaultForType: Option[TypeDefault[H]] = None,
        multiArgParser: MultiArgParser[H],
        tailFactory: ParserFactory[T, DT]
    ): ParserFactory[FieldType[K, H] :: T, Defaults] =
      defaults => {
        args =>
          val (matchedArgs, restArgs) = args.argsByKey(Set(fieldName.value.name))
          val defaultValue = defaultForField.head(defaults) orElse defaultForType.map(_.value)
          val eitherValue =
            if (matchedArgs.isEmpty)
              defaultValue.toRight(s"Missing argument ${fieldName.value.name}")
            else
              multiArgParser.parse(matchedArgs)
          for {
            tailResult <- tailFactory.create(defaultForField tail defaults).parse(restArgs)
            value <- eitherValue
          }
            yield
              field[K](value) :: tailResult
      }
  }

  implicit def genericParser[A, Rep <: HList, K <: HList, Defaults <: HList](
      implicit
      generic: LabelledGeneric.Aux[A, Rep],
      defaults: Default.AsOptions.Aux[A, Defaults],
      parserFactory: ParserFactory[Rep, Defaults]
  ): Parser[A] = {
    args =>
      parserFactory.create(defaults()).parse(args).map(generic.from)
  }
}
