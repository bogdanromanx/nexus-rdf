package ch.epfl.bluebrain.nexus.rdf.derivation

import magnolia.{CaseClass, Magnolia, SealedTrait}

object semiauto {

  type Typeclass[T] = NxvDecoder[T]

  def combine[T](caseClass: CaseClass[Typeclass, T]): Typeclass[T] =
    MagnoliaDecoder.combine(caseClass)

  def dispatch[T](sealedTrait: SealedTrait[Typeclass, T]): Typeclass[T] =
    MagnoliaDecoder.dispatch(sealedTrait)

  def deriveNxvDecoder[T]: Typeclass[T] = macro Magnolia.gen[T]

}
