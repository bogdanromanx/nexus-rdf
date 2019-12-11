package ch.epfl.bluebrain.nexus.rdf.derivation

import java.util.UUID

import cats.implicits._
import ch.epfl.bluebrain.nexus.rdf.Iri.AbsoluteIri
import ch.epfl.bluebrain.nexus.rdf.Node.{IriNode, Literal}
import ch.epfl.bluebrain.nexus.rdf.cursor.GraphCursor
import io.circe.Json

import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.reflect.ClassTag
import scala.util.Try

trait NxvDecoder[A] { self =>

  def apply(cursor: GraphCursor): Either[DecodingFailure, A]

  def map[B](f: A => B): NxvDecoder[B] =
    (cursor: GraphCursor) => self.apply(cursor).map(f)

  def flatMap[B](f: A => NxvDecoder[B]): NxvDecoder[B] =
    (cursor: GraphCursor) =>
      self(cursor) match {
        case Right(a)    => f(a)(cursor)
        case l @ Left(_) => l.asInstanceOf[NxvDecoder.Result[B]]
      }
}

object NxvDecoder {

  type Result[A] = Either[DecodingFailure, A]

  implicit val stringNxvDecoder: NxvDecoder[String] = (cursor: GraphCursor) =>
    cursor.focus match {
      case Some(lit: Literal) if lit.isString => Right(lit.lexicalForm)
      case _                                  => Left(DecodingFailure("Unable to decode node as a literal String", cursor.history))
    }

  implicit val booleanNxvDecoder: NxvDecoder[Boolean] = (cursor: GraphCursor) =>
    cursor.focus match {
      case Some(lit: Literal) if lit.isBoolean =>
        lit.lexicalForm.toBooleanOption
          .toRight(DecodingFailure("Unable to decode node as a literal Boolean", cursor.history))
      case _ => Left(DecodingFailure("Unable to decode node as a literal Boolean", cursor.history))
    }

  implicit val intNxvDecoder: NxvDecoder[Int]       = numeric[Int](_.toIntOption)
  implicit val longNxvDecoder: NxvDecoder[Long]     = numeric[Long](_.toLongOption)
  implicit val floatNxvDecoder: NxvDecoder[Float]   = numeric[Float](_.toFloatOption)
  implicit val doubleNxvDecoder: NxvDecoder[Double] = numeric[Double](_.toDoubleOption)

  implicit val absoluteIriNxvDecoder: NxvDecoder[AbsoluteIri] = (cursor: GraphCursor) =>
    cursor.focus match {
      case Some(IriNode(iri)) => Right(iri)
      case _                  => Left(DecodingFailure("Unable to decode node as an AbsoluteIri", cursor.history))
    }

  implicit val uuidNxvDecoder: NxvDecoder[UUID] = (cursor: GraphCursor) =>
    cursor.focus match {
      case Some(lit: Literal) if lit.isString =>
        Try(UUID.fromString(lit.lexicalForm)).toEither.left
          .map(_ => DecodingFailure("Unable to decode node as an UUID", cursor.history))
      case _ => Left(DecodingFailure("Unable to decode node as an UUID", cursor.history))
    }

  implicit val finiteDurationNxvDecoder: NxvDecoder[FiniteDuration] = (cursor: GraphCursor) =>
    cursor.focus match {
      case Some(lit: Literal) if lit.isString =>
        Try(Duration(lit.lexicalForm)).toEither match {
          case Right(fd: FiniteDuration) => Right(fd)
          case _                         => Left(DecodingFailure("Unable to decode node as a FiniteDuration", cursor.history))
        }
      case _ => Left(DecodingFailure("Unable to decode node as a FiniteDuration", cursor.history))
    }

  implicit val durationNxvDecoder: NxvDecoder[Duration] = (cursor: GraphCursor) =>
    cursor.focus match {
      case Some(lit: Literal) if lit.isString =>
        Try(Duration(lit.lexicalForm)).toEither.left
          .map(_ => DecodingFailure("Unable to decode node as a Duration", cursor.history))
      case _ => Left(DecodingFailure("Unable to decode node as a Duration", cursor.history))
    }

  implicit val jsonNxvDecoder: NxvDecoder[Json] = (cursor: GraphCursor) =>
    cursor.focus match {
      case Some(lit: Literal) if lit.isString =>
        io.circe.parser
          .parse(lit.lexicalForm)
          .leftMap(_ => DecodingFailure("Unable to decode subgraph as a Json", cursor.history))
      case _ => Left(DecodingFailure("Unable to decode subgraph as a Json", cursor.history))
    }

  implicit def setNxvDecoder[A](implicit A: NxvDecoder[A]): NxvDecoder[Set[A]] =
    (cursor: GraphCursor) => cursor.downSet.toList.traverse(c => A(c)).map(_.toSet)

  implicit def listNxvDecoder[A](implicit A: NxvDecoder[A]): NxvDecoder[List[A]] =
    (cursor: GraphCursor) => cursor.downList.traverse(c => A(c))

  implicit def arrayNxvDecoder[A: ClassTag](implicit A: NxvDecoder[A]): NxvDecoder[Array[A]] =
    (cursor: GraphCursor) => listNxvDecoder[A].apply(cursor).map(_.toArray[A])

  implicit def optionNxvDecoder[A](implicit A: NxvDecoder[A]): NxvDecoder[Option[A]] =
    (cursor: GraphCursor) =>
      cursor.focus match {
        case Some(_) => A(cursor).map(Some.apply)
        case None    => Right(None)
      }

  private def numeric[A](f: String => Option[A])(implicit A: ClassTag[A]): NxvDecoder[A] =
    (cursor: GraphCursor) =>
      cursor.focus match {
        case Some(lit: Literal) if lit.isNumeric =>
          f(lit.lexicalForm) match {
            case Some(a) => Right(a)
            case None =>
              Left(
                DecodingFailure(s"Unable to decode node as a literal ${A.runtimeClass.getSimpleName}", cursor.history)
              )
          }
        case _ =>
          Left(DecodingFailure(s"Unable to decode node as a literal ${A.runtimeClass.getSimpleName}", cursor.history))
      }

}
