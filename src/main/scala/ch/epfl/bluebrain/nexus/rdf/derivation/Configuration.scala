package ch.epfl.bluebrain.nexus.rdf.derivation

import ch.epfl.bluebrain.nexus.rdf.Iri.AbsoluteIri
import ch.epfl.bluebrain.nexus.rdf.Vocabulary

final case class Configuration(
    base: AbsoluteIri,
    discriminatorPredicate: AbsoluteIri,
    transformMemberNames: String => String,
    transformConstructorNames: String => String,
    idMemberName: String
)

object Configuration {
  val default: Configuration = Configuration(Vocabulary.nxv.base, Vocabulary.rdf.tpe.value, identity, identity, "id")
}

object defaults {
  implicit final val defaultConfiguration: Configuration = Configuration.default
}
