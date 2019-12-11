package ch.epfl.bluebrain.nexus.rdf

final case class GraphEncoder[A](encoder: Either[SubGraphEncoder[A], NodeEncoder[A]])

object GraphEncoder {

  implicit def graphEncoderFromSubGraphEncoder[A](implicit A: SubGraphEncoder[A]): GraphEncoder[A] =
    GraphEncoder(Left(A))

  implicit def graphEncoderFromNodeEncoder[A](implicit A: NodeEncoder[A]): GraphEncoder[A] =
    GraphEncoder(Right(A))

}