/*
 *  VideoSettings.scala
 *  (MutagenTx)
 *
 *  Copyright (c) 2015-2016 Hanns Holger Rutz. All rights reserved.
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
         secondsPerIteration = secondsPerIteration, secondsDecay = secondsDecay, secondsSkip = secondsSkip,
         chromosomeIndex = chromosomeIndex, plopDur = plopDur, speedLimit = speedLimit)
  }

  private final case class Impl(baseFile: File, width: Int, height: Int, framesPerSecond: Int,
                                secondsPerIteration: Double, secondsDecay: Double, secondsSkip: Double,
                                chromosomeIndex: Int, plopDur: Double, speedLimit: Double)
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

    var width               = 1080 // 1920
    var height              = 1920 // 1080
    var framesPerSecond     = 25
    var secondsPerIteration = 10.0 //  2.0
    var secondsDecay        = 20.0 // 10.0
    var secondsSkip         = 0.0
    var chromosomeIndex     = 0
    var plopDur             = 0.333
    var speedLimit          = 0.04
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
  def chromosomeIndex     : Int
  def plopDur             : Double
  def speedLimit          : Double
}
