/*
 *  VisualBit.scala
 *  (MutagenTx)
 *
 *  Copyright (c) 2015 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.mutagentx
package visual

object VisualBit {
  def apply(main: Visual, b: Bit)(implicit tx: S#Tx): VisualBit = impl.VisualBitImpl(main, b)
}
trait VisualBit extends VisualNode {
  var state: Boolean
}