package com.github.lavrov.cliche

case class TypeDefault[A](value: A)

object TypeDefault {

  implicit def optionDefault[T] = TypeDefault[Option[T]](None)

  implicit def implicitLookup[A](implicit dv: TypeDefault[A]): Option[TypeDefault[A]] = Some(dv)
}

