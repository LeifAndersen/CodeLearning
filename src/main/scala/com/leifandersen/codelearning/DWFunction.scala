package com.leifandersen.codelearning

import scala.io.Source
import scala.io.Source.fromFile

import de.libalf.BasicAutomaton
import de.libalf.Knowledgebase
import de.libalf.LearningAlgorithm
import de.libalf.LibALFFactory
import de.libalf.LibALFFactory.Algorithm
import de.libalf.jni.JNIFactory

object DWFunction extends CodeFunction {

  def code2word(in: String): Int = in match {
    case "clear"                    => 0
    case "exists"                   => 1
    case "put"                      => 2
    case "putForeign"               => 3
    case "get"                      => 4
    case "getlist"                  => 5
    case "print"                    => 6
    case "cleanForeign"             => 7
    case "decrementScrubCount"      => 8
    case "setScrubCount"            => 9
    case "scrub"                    => 10
    case "initializeScrubs"         => 11
    case "logMemoryUse"             => 12
    case "getVarLabelMatlTriples"   => 13
    case s if s.startsWith("0x0")   => 0
    case s if s.startsWith("0x1")   => 1
    case s if s.startsWith("0x2")   => 2
    case s if s.startsWith("0x3")   => 3
    case s if s.startsWith("0x4")   => 4
    case s if s.startsWith("0x5")   => 5
    case s if s.startsWith("0x6")   => 6
    case s if s.startsWith("0x7")   => 7
    case s if s.startsWith("0x8")   => 8
    case s if s.startsWith("0x9")   => 9
    case s if s.startsWith("0")     => 0
    case s if s.startsWith("1")     => 1
    case s if s.startsWith("2")     => 2
    case s if s.startsWith("3")     => 3
    case s if s.startsWith("4")     => 4
    case s if s.startsWith("5")     => 5
    case s if s.startsWith("6")     => 6
    case s if s.startsWith("7")     => 7
    case s if s.startsWith("8")     => 8
    case s if s.startsWith("9")     => 9
    case _                          => 16
  }

  def word2code(in: Int): String = in match {
    case 0                          => "clear"
    case 1                          => "exists"
    case 2                          => "put"
    case 3                          => "putForeign"
    case 4                          => "get"
    case 5                          => "getlist"
    case 6                          => "print"
    case 7                          => "cleanForeign"
    case 8                          => "decrementScrubCount"
    case 9                          => "setScrubCount"
    case 10                         => "scrub"
    case 11                         => "initializeScrubs"
    case 12                         => "logMemoryUse"
    case 13                         => "getVarLabelMatlTriples"
    case _                          => "error\n"
  }

}
