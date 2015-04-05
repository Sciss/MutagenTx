/*
 *  package.scala
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

package de.sciss

import de.sciss.lucre.confluent.reactive.ConfluentReactive
import de.sciss.lucre.event.Durable

package object mutagentx {
  type S = ConfluentReactive
  type D = Durable
}
