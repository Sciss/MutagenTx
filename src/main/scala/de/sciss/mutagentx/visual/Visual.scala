/*
 *  Visual.scala
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

import de.sciss.lucre.swing.{View, deferTx}
import de.sciss.lucre.swing.impl.ComponentHolder

import scala.swing.Component

object Visual {
  def apply(a: Algorithm)(implicit tx: S#Tx): Visual = {
    new Impl().init()
  }

  private final class Impl() extends Visual with ComponentHolder[Component] {
    def init()(implicit tx: S#Tx): this.type = {
      deferTx(guiInit())
      this
    }

    private def guiInit(): Unit = {

    }

    def dispose()(implicit tx: S#Tx): Unit = ()
  }
}
trait Visual extends View[S]