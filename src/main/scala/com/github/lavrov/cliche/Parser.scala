package com.github.lavrov.cliche

import shapeless._, ops.hlist.IsHCons
import shapeless.labelled.{FieldType, field}

trait Parser[A] {
  def parse(args: CommandLineArgs): Either[String, A]
}

object Parser {

  def apply[A](implicit parser: Parser[A]): Parser[A] = parser

  trait ParserFactory[A, Defaults, Recurse] {
    def create(defaults: Defaults, recurse: Recurse): Parser[A]
  }


  object ParserFactory {

    implicit def hNilParser[Defaults <: HList, Recurses <: HList]: ParserFactory[HNil, Defaults, Recurses] =
      (_, _) => {
        case CommandLineArgs(Nil) => Right(HNil)
        case _ => Left("There are not parsed args")
      }

    implicit def hConsParser[K <: Symbol, H, T <: HList, DH, Defaults <: HList, DT <: HList, Recurses <: HList, RT <: HList](
        implicit
        fieldName: Witness.Aux[K],
        defaultForField: IsHCons.Aux[Defaults, Option[H], DT],
        defaultForType: Option[TypeDefault[H]] = None,
        recurseForField: IsHCons.Aux[Recurses, None.type, RT],
        multiArgParser: MultiArgParser[H],
        tailFactory: ParserFactory[T, DT, RT]
    ): ParserFactory[FieldType[K, H] :: T, Defaults, Recurses] = {
      (defaults, recurse) =>
        args =>
          val (matchedArgs, restArgs) = args.argsByKey(Set(fieldName.value.name))
          val defaultValue = defaultForField.head(defaults) orElse defaultForType.map(_.value)
          val eitherValue =
            if (matchedArgs.isEmpty)
              defaultValue.toRight(s"Missing argument ${fieldName.value.name}")
            else
              multiArgParser.parse(matchedArgs)
          for {
            value <- eitherValue
            tailResult <- tailFactory.create(defaultForField tail defaults, recurseForField tail recurse).parse(restArgs)
          }
            yield
              field[K](value) :: tailResult
      }
  }

  implicit def hConsRecursiveParser[K <: Symbol, H, T <: HList, DH, Defaults <: HList, DT <: HList, Recurses <: HList, RT <: HList](
      implicit
      defaultForField: IsHCons.Aux[Defaults, Option[H], DT],
      recurseForField: IsHCons.Aux[Recurses, Some[Recurse], RT],
      parser: Parser[H],
      tailFactory: ParserFactory[T, DT, RT]
  ): ParserFactory[FieldType[K, H] :: T, Defaults, Recurses] = {
    (defaults, recurse) =>
      args =>
        for {
          value <- parser.parse(args)
          tailResult <- tailFactory.create(defaultForField tail defaults, recurseForField tail recurse).parse(args)
        }
          yield
            field[K](value) :: tailResult
    }

  implicit def genericParser[A, Rep <: HList, K <: HList, Defaults <: HList, Recurses <: HList](
      implicit
      generic: LabelledGeneric.Aux[A, Rep],
      defaults: Default.AsOptions.Aux[A, Defaults],
      recurses: Annotations.Aux[Recurse, A, Recurses],
      parserFactory: ParserFactory[Rep, Defaults, Recurses]
  ): Parser[A] = {
    args =>
      parserFactory.create(defaults(), recurses()).parse(args).map(generic.from)
  }
}
