package ch.epfl.bluebrain.nexus.rdf

import ch.epfl.bluebrain.nexus.rdf.Node.{BNode, IriOrBNode}

trait SubGraphEncoder[A] extends Serializable { self =>

  def apply(a: A): (IriOrBNode, Graph)

  final def contramap[B](f: B => A): SubGraphEncoder[B] = new SubGraphEncoder[B] {
    def apply(a: B): (IriOrBNode, Graph) = self(f(a))
  }

  final def mapNode(f: IriOrBNode => IriOrBNode): SubGraphEncoder[A] = new SubGraphEncoder[A] {
    def apply(a: A): (IriOrBNode, Graph) = {
      val (n, g) = self(a)
      val newNode = f(n)
      (newNode, g.replaceNode(n, newNode))
    }
  }

  final def mapGraph(f: Graph => Graph): SubGraphEncoder[A] = new SubGraphEncoder[A] {
    def apply(a: A): (IriOrBNode, Graph) = {
      val (n, g) = self(a)
      (n, f(g))
    }
  }
}

object SubGraphEncoder {

  @inline
  final def apply[A](implicit instance: SubGraphEncoder[A]): SubGraphEncoder[A] = instance

  final def instance[A](f: A => (IriOrBNode, Graph)): SubGraphEncoder[A] = new SubGraphEncoder[A] {
    final def apply(a: A): (IriOrBNode, Graph) = f(a)
  }

  implicit final def subGraphEncodeOption[A](implicit A: SubGraphEncoder[A]): SubGraphEncoder[Option[A]] =
    new SubGraphEncoder[Option[A]] {
      override def apply(a: Option[A]): (IriOrBNode, Graph) = a match {
        case Some(value) => A(value)
        case None => (BNode(), Graph())
      }
    }
}