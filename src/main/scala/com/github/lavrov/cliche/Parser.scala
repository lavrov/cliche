package com.github.lavrov.cliche

import shapeless._, ops.hlist.IsHCons
import shapeless.labelled.{FieldType, field}

trait Parser[A] {
  def parse(args: CommandLineArgs): Either[String, ParserResult[A]]
}

case class ParserResult[A](value: A, unparsed: CommandLineArgs = CommandLineArgs(Nil))

object Parser {

  def apply[A](implicit parser: Parser[A]): Parser[A] = parser

  trait ParserFactory[A, Defaults, Recurse] {
    def create(defaults: Defaults, recurse: Recurse): Parser[A]
  }


  object ParserFactory {

    implicit def hNilParser[Defaults <: HList, Recurses <: HList]: ParserFactory[HNil, Defaults, Recurses] =
      (_, _) => {
        case CommandLineArgs(Nil) => Right(ParserResult(HNil))
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
          eitherValue.flatMap { value =>
             tailFactory.create(defaultForField tail defaults, recurseForField tail recurse).parse(restArgs).map {
               case ParserResult(tailResult, unparsed) =>
                 ParserResult(field[K](value) :: tailResult, unparsed)
             }
          }
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
       parser.parse(args).flatMap {
         case ParserResult(value, unparsed) =>
           tailFactory.create(defaultForField tail defaults, recurseForField tail recurse).parse(unparsed).map {
             case ParserResult(tailResult, unparsedTail) =>
               ParserResult(field[K](value) :: tailResult, unparsedTail)
           }
       }
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
        case  ParserResult(genRep, unparsed) =>
          ParserResult(generic from genRep, unparsed)
      }
  }
}
