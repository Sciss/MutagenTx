package de.sciss

import de.sciss.lucre.confluent.Confluent
import de.sciss.lucre.stm.Durable

package object mutagentx {
  type S = Confluent
  type D = Durable
}
