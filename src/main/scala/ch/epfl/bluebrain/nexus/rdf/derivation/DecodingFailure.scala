package ch.epfl.bluebrain.nexus.rdf.derivation

import ch.epfl.bluebrain.nexus.rdf.cursor.CursorOp

final case class DecodingFailure(message: String, history: List[CursorOp])