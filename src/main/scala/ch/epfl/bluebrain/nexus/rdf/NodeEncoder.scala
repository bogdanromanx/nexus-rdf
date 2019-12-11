package ch.epfl.bluebrain.nexus.rdf

import java.util.UUID

import ch.epfl.bluebrain.nexus.rdf.Node.Literal

trait NodeEncoder[A] extends Serializable { self =>

  def apply(a: A): Node

  final def contramap[B](f: B => A): NodeEncoder[B] =
    (a: B) => self(f(a))

  final def mapNode(f: Node => Node): NodeEncoder[A] =
    (a: A) => f(self(a))

}

object NodeEncoder {

  implicit final val nodeEncodeString: NodeEncoder[String]   = (a: String) => Literal(a)
  implicit final val nodeEncodeBoolean: NodeEncoder[Boolean] = (a: Boolean) => Literal(a)
  implicit final val nodeEncodeInt: NodeEncoder[Int]         = (a: Int) => Literal(a)
  implicit final val nodeEncodeLong: NodeEncoder[Long]       = (a: Long) => Literal(a)
  implicit final val nodeEncodeFloat: NodeEncoder[Float]     = (a: Float) => Literal(a)
  implicit final val nodeEncodeDouble: NodeEncoder[Double]   = (a: Double) => Literal(a)

  implicit final val nodeEncodeUUID: NodeEncoder[UUID] = nodeEncodeString.contramap(_.toString)

}
