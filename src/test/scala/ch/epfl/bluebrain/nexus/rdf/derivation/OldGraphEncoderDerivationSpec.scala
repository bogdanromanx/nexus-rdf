package ch.epfl.bluebrain.nexus.rdf.derivation

import java.util.UUID

import ch.epfl.bluebrain.nexus.rdf.Iri.AbsoluteIri
import ch.epfl.bluebrain.nexus.rdf.derivation.OldGraphEncoderDerivationSpec.View
import ch.epfl.bluebrain.nexus.rdf.derivation.OldGraphEncoderDerivationSpec.View.ElasticSearchView
import ch.epfl.bluebrain.nexus.rdf.derivation.encoder.semiauto._
import ch.epfl.bluebrain.nexus.rdf.syntax._
import ch.epfl.bluebrain.nexus.rdf.{OldGraphEncoder, RdfSpec}
import com.github.ghik.silencer.silent
import io.circe.Json
import io.circe.parser.parse

class OldGraphEncoderDerivationSpec extends RdfSpec {

  private val mapping: Json = parse(
    "{\"properties\":{\"@type\":{\"type\":\"keyword\",\"copy_to\":\"_all_fields\"},\"@id\":{\"type\":\"keyword\",\"copy_to\":\"_all_fields\"},\"_rev\":{\"type\":\"long\",\"copy_to\":\"_all_fields\"},\"_deprecated\":{\"type\":\"boolean\",\"copy_to\":\"_all_fields\"},\"_createdAt\":{\"type\":\"date\",\"copy_to\":\"_all_fields\"},\"_updatedAt\":{\"type\":\"date\",\"copy_to\":\"_all_fields\"},\"_createdBy\":{\"type\":\"keyword\",\"copy_to\":\"_all_fields\"},\"_updatedBy\":{\"type\":\"keyword\",\"copy_to\":\"_all_fields\"},\"_constrainedBy\":{\"type\":\"keyword\",\"copy_to\":\"_all_fields\"},\"_project\":{\"type\":\"keyword\",\"copy_to\":\"_all_fields\"},\"_self\":{\"type\":\"keyword\",\"copy_to\":\"_all_fields\"},\"_incoming\":{\"type\":\"keyword\",\"copy_to\":\"_all_fields\"},\"_outgoing\":{\"type\":\"keyword\",\"copy_to\":\"_all_fields\"},\"_original_source\":{\"type\":\"text\",\"copy_to\":\"_all_fields\"},\"_bytes\":{\"type\":\"long\",\"copy_to\":\"_all_fields\"},\"_mediaType\":{\"type\":\"keyword\",\"copy_to\":\"_all_fields\"},\"_location\":{\"type\":\"keyword\",\"copy_to\":\"_all_fields\"},\"_filename\":{\"type\":\"keyword\",\"copy_to\":\"_all_fields\"},\"_digest\":{\"type\":\"nested\",\"properties\":{\"_algorithm\":{\"type\":\"keyword\",\"copy_to\":\"_all_fields\"},\"_value\":{\"type\":\"keyword\",\"copy_to\":\"_all_fields\"}}},\"_storage\":{\"type\":\"nested\",\"properties\":{\"_rev\":{\"type\":\"long\",\"copy_to\":\"_all_fields\"},\"@id\":{\"type\":\"keyword\",\"copy_to\":\"_all_fields\"}}},\"_all_fields\":{\"type\":\"text\"}},\"dynamic\":false}"
  ).rightValue

  "A GraphEncoder" should {
    "be derived correctly" in {
      val view = ElasticSearchView(
        id = url"http://example.com/id".value,
        uuid = Some(UUID.fromString("3aa14a1a-81e7-4147-8306-136d8270bb01")),
        mapping = mapping,
        resourceSchemas = Set(nxv"Schema".value, nxv"Resource".value),
        resourceTypes = Set(nxv"MyType".value, nxv"MyType2".value),
        resourceTag = Some("one"),
        sourceAsText = Some(true)
      )
      println(implicitly[OldGraphEncoder[View]].apply(view))
    }
  }

}

object OldGraphEncoderDerivationSpec {

  sealed trait View extends Product with Serializable

  object View {

    final case class ElasticSearchView(
        id: AbsoluteIri,
        uuid: Option[UUID],
        mapping: Json,
        resourceSchemas: Set[AbsoluteIri],
        resourceTypes: Set[AbsoluteIri],
        resourceTag: Option[String] = None,
        sourceAsText: Option[Boolean] = Some(true)
    ) extends View

    final case class AggregateElasticSearchView(
        id: AbsoluteIri,
        uuid: Option[UUID],
        views: List[ViewRef]
    ) extends View

    final case class ViewRef(project: String, viewId: AbsoluteIri)
    object ViewRef {
      @silent
      implicit val viewRefGraphEncoder = deriveGraphEncoder[ViewRef]
    }

    @silent
    implicit val viewGraphEncoder = deriveGraphEncoder[View]
  }

}
