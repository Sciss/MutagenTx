package de.sciss.mutagentx

case class FeatureExtractionFailed(cause: Throwable) extends Exception(cause)