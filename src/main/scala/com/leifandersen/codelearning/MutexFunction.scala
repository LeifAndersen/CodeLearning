package com.leifandersen.codelearning

import scala.io.Source
import scala.io.Source.fromFile

import de.libalf.BasicAutomaton
import de.libalf.Knowledgebase
import de.libalf.LearningAlgorithm
import de.libalf.LibALFFactory
import de.libalf.LibALFFactory.Algorithm
import de.libalf.jni.JNIFactory

object MutexFunction extends CodeFunction {

  def code2word(in: String): Int = in match {
    case "Mutex"                    => 0
    case "~Mutex"                   => 1
    case "unlock"                   => 2
    case "lock"                     => 3
    case "tryLock"                  => 4
    case _                          => {
      printf("Invalid function: %s\n", in)
      return 16
    }
  }

  def word2code(in: Int): String = in match {
    case 0 => "Mutex"
    case 1 => "~Mutex"
    case 2 => "unlock"
    case 3 => "lock"
    case 4 => "tryLock"
    case _ => "error"
  }
}
