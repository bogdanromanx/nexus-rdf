package ch.epfl.bluebrain.nexus.rdf

import cats.kernel.Eq
import cats.syntax.show._
import ch.epfl.bluebrain.nexus.rdf.Iri._
import org.scalatest.{EitherValues, Inspectors, Matchers, WordSpecLike}

class RelativeIriSpec extends WordSpecLike with Matchers with Inspectors with EitherValues {

  "A RelativeIri" should {
    "be parsed correctly" in {
      val cases = List(
        "//me:me@hOst:443/a/b?a&e=f&b=c#frag" -> "//me:me@host:443/a/b?a&b=c&e=f#frag",
        "//me:me@hOst#frag"                   -> "//me:me@host#frag",
        "/some/:/path"                        -> "/some/:/path",
        "a/../b/./c"                          -> "/b/c",
        "../../../"                           -> "../../../",
        "/../../"                             -> "/",
        "/:/some/path"                        -> "/:/some/path",
        "some/:/path"                         -> "some/:/path",
        "?q=v"                                -> "?q=v",
        "#frag"                               -> "#frag",
        "//hOst:443/a/b/../c"                 -> "//host:443/a/c",
        "//1.2.3.4:80/a%C2%A3/b%C3%86c//:://" -> "//1.2.3.4:80/a£/bÆc//:://",
        "//1.2.3.4:80/a%C2%A3/b%C3%86c//:://" -> "//1.2.3.4:80/a£/bÆc//:://",
        "//1.2.3.4:80/a%C2%A3/b%C3%86c//:://" -> "//1.2.3.4:80/a£/bÆc//:://"
      )
      forAll(cases) {
        case (in, expected) => RelativeIri(in).right.value.show shouldEqual expected
      }
    }

    "fail to parse from string" in {
      val cases = List(
        "http://me:me@hOst:443/a/b?a&e=f&b=c#frag",
        ":/some/path",
        " ",
        ""
      )
      forAll(cases) {
        case (in) => RelativeIri(in).left.value should not be 'empty
      }
    }
    val withHash = Iri.relative("//1.2.3.4:80/a%C2%A3/b%C3%86c//:://#hash").right

    "be relative" in {
      withHash.value.isRelative shouldEqual true
    }

    "return an optional self" in {
      withHash.value.asRelative shouldEqual Some(withHash.value)
    }

    "not be an Urn" in {
      withHash.value.isUrn shouldEqual false
    }

    "not be an Url" in {
      withHash.value.isUrl shouldEqual false
    }

    "not return a urn" in {
      withHash.value.asUrn shouldEqual None
    }

    "not return a url" in {
      withHash.value.asUrl shouldEqual None
    }

    "eq" in {
      val lhs = RelativeIri("a/./b/../?q=asd#1").right.value
      val rhs = RelativeIri("a/?q=asd#1").right.value
      Eq.eqv(lhs, rhs) shouldEqual true
    }
  }
}
