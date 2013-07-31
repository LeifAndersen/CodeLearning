package com.leifandersen.codelearning

import scala.io.Source
import scala.io.Source.fromFile

import de.libalf.BasicAutomaton
import de.libalf.Knowledgebase
import de.libalf.LearningAlgorithm
import de.libalf.LibALFFactory
import de.libalf.LibALFFactory.Algorithm
import de.libalf.jni.JNIFactory

trait CodeFunction {
  def code2word(in: String): Int
  def word2code(in: Int): String
}
