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
    case s if s.startsWith("0x0")   => 14
    case s if s.startsWith("0x1")   => 15
    case s if s.startsWith("0x2")   => 16
    case s if s.startsWith("0x3")   => 17
    case s if s.startsWith("0x4")   => 18
    case s if s.startsWith("0x5")   => 19
    case s if s.startsWith("0x6")   => 20
    case s if s.startsWith("0x7")   => 21
    case s if s.startsWith("0x8")   => 22
    case s if s.startsWith("0x9")   => 23
    case s if s.startsWith("0")     => 14
    case s if s.startsWith("1")     => 15
    case s if s.startsWith("2")     => 16
    case s if s.startsWith("3")     => 17
    case s if s.startsWith("4")     => 18
    case s if s.startsWith("5")     => 19
    case s if s.startsWith("6")     => 20
    case s if s.startsWith("7")     => 21
    case s if s.startsWith("8")     => 22
    case s if s.startsWith("9")     => 23
    case _                          => 24
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
    case 14                         => "0"
    case 15                         => "1"
    case 16                         => "2"
    case 17                         => "3"
    case 18                         => "4"
    case 19                         => "5"
    case 20                         => "6"
    case 21                         => "7"
    case 22                         => "8"
    case 23                         => "9"
    case _                          => "error"
  }

  def wordSize: Int = 25
}
