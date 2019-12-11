package ch.epfl.bluebrain.nexus.rdf.derivation

import ch.epfl.bluebrain.nexus.rdf.Node.{IriNode, IriOrBNode}
import ch.epfl.bluebrain.nexus.rdf.{Graph, OldGraphEncoder, Node, RootedGraph}
import com.github.ghik.silencer.silent
import magnolia.{CaseClass, SealedTrait}

private[derivation] object MagnoliaEncoder {

  @silent
  def combine[A](
      caseClass: CaseClass[OldGraphEncoder, A]
  )(implicit config: Configuration): OldGraphEncoder.Aux[A, RootedGraph] = {
    val paramPredicateLookup = caseClass.parameters.map { p =>
      val idAnnotation = p.annotations.collectFirst {
        case ann: Id => ann
      }
      idAnnotation match {
        case Some(ann) => p.label -> ann.value
        case None      => p.label -> (config.base + config.transformMemberNames(p.label))
      }
    }.toMap

    if (paramPredicateLookup.values.toList.distinct.size != caseClass.parameters.length) {
      throw DerivationError("Duplicate key detected after applying transformation function for case class parameters")
    }

    val paramIdLookup = caseClass.parameters.find(_.label == config.idMemberName)

    paramIdLookup match {
      case Some(id) =>
        val rest = caseClass.parameters.filterNot(_ == id)
        new OldGraphEncoder[A] {
          override type Out = RootedGraph
          override def apply(a: A): Out = {
            val rootNode: IriOrBNode = id.typeclass.apply(id.dereference(a)) match {
              case n: IriOrBNode => n
              case other =>
                throw DerivationError(
                  s"The id field for class '${caseClass.typeName.short}' does not encode to a IriOrBNode, but ${other.getClass.getCanonicalName}"
                )
            }
            rest.foldLeft(RootedGraph(rootNode, Graph())) { (acc, el) =>
              val predicate = IriNode(paramPredicateLookup(el.label))
              el.typeclass.apply(el.dereference(a)) match {
                case n: Node =>
                  RootedGraph(acc.rootNode, acc.add(acc.rootNode, predicate, n))
                case Some(n: Node) =>
                  RootedGraph(acc.rootNode, acc.add(acc.rootNode, predicate, n))
                case g: RootedGraph =>
                  RootedGraph(acc.rootNode, acc ++ g ++ Graph().add(acc.rootNode, predicate, g.rootNode))
                case Some(g: RootedGraph) =>
                  RootedGraph(acc.rootNode, acc ++ g ++ Graph().add(acc.rootNode, predicate, g.rootNode))
                case None => acc
                case other =>
                  throw DerivationError(
                    s"The '${el.label}' field for class '${caseClass.typeName.short}' does not encode to a Node or a RootedGraph, but ${other.getClass.getCanonicalName}"
                  )
              }
            }
          }
        }
      case None =>
        val rootNode = Node.blank
        new OldGraphEncoder[A] {
          override type Out = RootedGraph
          override def apply(a: A): Out = {
            caseClass.parameters.foldLeft(RootedGraph(rootNode, Graph())) { (acc, el) =>
              val predicate = IriNode(paramPredicateLookup(el.label))
              el.typeclass.apply(el.dereference(a)) match {
                case n: Node =>
                  RootedGraph(acc.rootNode, acc.add(acc.rootNode, predicate, n))
                case g: RootedGraph =>
                  RootedGraph(acc.rootNode, acc ++ g ++ Graph().add(acc.rootNode, predicate, g.rootNode))
                case other =>
                  throw DerivationError(
                    s"The '${el.label}' field for class '${caseClass.typeName.short}' does not encode to a Node or a RootedGraph, but ${other.getClass.getCanonicalName}"
                  )
              }
            }
          }
        }
    }
  }

  def dispatch[A](
      sealedTrait: SealedTrait[OldGraphEncoder, A]
  )(implicit config: Configuration): OldGraphEncoder.Aux[A, RootedGraph] =
    new OldGraphEncoder[A] {
      override type Out = RootedGraph
      override def apply(a: A): Out = {
        sealedTrait.dispatch(a) { subType =>
          subType.typeclass.apply(subType.cast(a)) match {
            case g: RootedGraph =>
              RootedGraph(
                g.rootNode,
                g.add(g.rootNode, IriNode(config.discriminatorPredicate), IriNode(config.base + subType.typeName.short))
              )
            case other =>
              throw DerivationError(
                s"Members of '${sealedTrait.typeName}' should only encode to a RootedGraph, but '${subType.typeName}' encodes to ${other.getClass.getCanonicalName}"
              )
          }
        }
      }
    }

}
