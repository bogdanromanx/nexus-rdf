package ch.epfl.bluebrain.nexus.rdf

import java.util.UUID

import ch.epfl.bluebrain.nexus.rdf.Iri.AbsoluteIri
import ch.epfl.bluebrain.nexus.rdf.Node.{IriNode, Literal}

trait OldGraphEncoder[A] extends Serializable { self =>
  type Out

  def apply(a: A): Out

  final def contramap[B](f: B => A): OldGraphEncoder.Aux[B, Out] = new OldGraphEncoder[B] {
    override type Out = self.Out
    override def apply(a: B): Out = self(f(a))
  }
}

object OldGraphEncoder {

  type Aux[A, Out0] = OldGraphEncoder[A] {
    type Out = Out0
  }

  final def apply[A](implicit instance: OldGraphEncoder[A]): OldGraphEncoder[A] = instance

  final def node[A](f: A => Node): OldGraphEncoder.Aux[A, Node] = new OldGraphEncoder[A] {
    override type Out = Node
    override def apply(a: A): Out = f(a)
  }

  implicit final val graphEncodeString: OldGraphEncoder[String]           = node(Literal(_))
  implicit final val graphEncodeBoolean: OldGraphEncoder[Boolean]         = node(Literal(_))
  implicit final val graphEncodeInt: OldGraphEncoder[Int]                 = node(Literal(_))
  implicit final val graphEncodeLong: OldGraphEncoder[Long]               = node(Literal(_))
  implicit final val graphEncodeFloat: OldGraphEncoder[Float]             = node(Literal(_))
  implicit final val graphEncodeDouble: OldGraphEncoder[Double]           = node(Literal(_))
  implicit final val graphEncodeUUID: OldGraphEncoder[UUID]               = node(uuid => Literal(uuid.toString))
  implicit final val graphEncodeAbsoluteIri: OldGraphEncoder[AbsoluteIri] = node(IriNode(_))

  implicit final def graphEncodeOption[A](implicit A: OldGraphEncoder[A]): OldGraphEncoder.Aux[Option[A], Option[A.Out]] =
    new OldGraphEncoder[Option[A]] {
      override type Out = Option[A.Out]
      override def apply(a: Option[A]): Out =
        a.map(A.apply)
    }
}
