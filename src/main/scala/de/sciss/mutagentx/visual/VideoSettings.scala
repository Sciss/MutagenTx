/*
 *  VideoSettings.scala
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

package de.sciss.mutagentx.visual

import de.sciss.file.File

import scala.language.implicitConversions

object VideoSettings {
  def apply() = new Builder

  implicit def build(b: Builder): VideoSettings = {
    import b._
    Impl(baseFile = baseFile, width = width, height = height, framesPerSecond = framesPerSecond,
         secondsPerIteration = secondsPerIteration, secondsDecay = secondsDecay, secondsSkip = secondsSkip)
  }

  private final case class Impl(baseFile: File, width: Int, height: Int, framesPerSecond: Int,
                                secondsPerIteration: Double, secondsDecay: Double, secondsSkip: Double)
    extends VideoSettings {

    override def productPrefix = "VideoSetting"
  }

  final class Builder extends VideoSettings {
    private var _baseFile: File = _
    def baseFile: File = {
      if (_baseFile == null) throw new IllegalStateException("baseFile has not been set")
      _baseFile
    }

    def baseFile_=(value: File): Unit = _baseFile = value

    var width               = 1920
    var height              = 1080
    var framesPerSecond     = 25
    var secondsPerIteration = 2.0
    var secondsDecay        = 10.0
    var secondsSkip         = 0.0
  }
}
trait VideoSettings {
  def baseFile            : File
  def width               : Int
  def height              : Int
  def framesPerSecond     : Int
  def secondsPerIteration : Double
  def secondsDecay        : Double
  def secondsSkip         : Double
}
