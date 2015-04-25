/*
 *  Edge.scala
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

import de.sciss.lucre.stm.Identifiable

object Edge {

}
trait Edge extends Identifiable[S#Tx] {
  def source: S#Var[Vertex]
  def target: S#Var[Vertex]
}
