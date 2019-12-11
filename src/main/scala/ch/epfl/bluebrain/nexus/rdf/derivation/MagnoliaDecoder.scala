package ch.epfl.bluebrain.nexus.rdf.derivation

import ch.epfl.bluebrain.nexus.rdf.Vocabulary
import ch.epfl.bluebrain.nexus.rdf.cursor.GraphCursor
import ch.epfl.bluebrain.nexus.rdf.syntax.node.unsafe._
import magnolia.{CaseClass, SealedTrait}

private[derivation] object MagnoliaDecoder {

  private[derivation] def combine[A](caseClass: CaseClass[NxvDecoder, A]): NxvDecoder[A] = new NxvDecoder[A] {
    override def apply(cursor: GraphCursor): Either[DecodingFailure, A] = {
      caseClass.constructMonadic { p =>
        if(p.label == "id") {
          p.typeclass.apply(cursor)
        } else {
          val next = cursor.downField(_ == nxv"${p.label}")
          next.focus match {
            case None    => p.default.fold(p.typeclass.apply(next))(Right[DecodingFailure, p.PType])
            case Some(_) => p.typeclass.apply(next)
          }
        }
      }
    }
  }

  private[derivation] def dispatch[A](sealedTrait: SealedTrait[NxvDecoder, A]): NxvDecoder[A] = new NxvDecoder[A] {
    override def apply(cursor: GraphCursor): Either[DecodingFailure, A] = {
      cursor.downField(_ == Vocabulary.rdf.tpe).values match {
        case Some(values) =>
          val vset = values.toSet
          sealedTrait.subtypes.find(st => vset.contains(nxv"${st.typeName.short}")) match {
            case Some(st) => st.typeclass.apply(cursor)
            case None =>
              Left(
                DecodingFailure(s"Unable to find type discriminator for ${sealedTrait.typeName.short}", cursor.history)
              )
          }
        case None =>
          Left(DecodingFailure(s"Unable to find type discriminator for ${sealedTrait.typeName.short}", cursor.history))
      }
    }
  }

}
