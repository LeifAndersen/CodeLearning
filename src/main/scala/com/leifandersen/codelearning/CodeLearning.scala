package com.leifandersen.codelearning

import scala.io.Source
import scala.io.Source.fromFile
import scala.sys.process.Process

import java.io.File
import java.io.PrintWriter

import de.libalf.BasicAutomaton
import de.libalf.Knowledgebase
import de.libalf.LearningAlgorithm
import de.libalf.LibALFFactory
import de.libalf.LibALFFactory.Algorithm
import de.libalf.jni.JNIFactory

object CodeLearning extends App {

  def file2database(in: String, base: Knowledgebase, inLanguage: Boolean, maxString: Int,
                  functions: CodeFunction) {
    var samples = CodeSampleDatabase()
    val file = fromFile(in)
    val str = file.toList.mkString

    for(i <- str.split('\n')) {
      samples.extendRaw(i)
    }
    for(sample <- samples) {
      var word = List[Int]()
      var counter = 0
      for(line <- sample._2) {
        if(counter < maxString || maxString <= 0) {
          word ++= line2word(line, functions)
        }
        counter += 1
      }
      if(maxString <= 0) {
        base.add_knowledge(word.toArray, inLanguage)
      } else {
        base.add_knowledge(word.take(maxString).toArray, inLanguage)
      }
    }
  }

  def line2word(in: String, functions: CodeFunction): List[Int] = {
    val wordSize = functions.wordSize
    var line = CallParser(in)
    var sum = 0
    var multiplier = 1
    for(arguement <- line) {
      sum += functions.code2word(arguement)*multiplier
      multiplier *= wordSize
    }
    return List(sum)
    //return List(functions.code2word(command(0)))
  }

  def command2word(in: String, functions: CodeFunction): List[Int] = {
    var command = in.split('(')
    return List(functions.code2word(command(0)))
  }


  def word2line(in: Int, functions: CodeFunction): String = {
    val wordSize = functions.wordSize
    var number = in
    var command = functions.word2code(number%wordSize)
    number /= wordSize
    command += "("
    while (number > 0) {
      command += functions.word2code(number%wordSize)
      number /= wordSize
      if(number > 0) {
        command += ","
      }
    }
    command += ");"
    return command
    //return functions.word2code(in)
  }

  def generateAutomaton(): BasicAutomaton = {
    // Set up Alphabet
    val numFunctions = 10
    val numPositions = 10
    val numValues = 10
    //var alphabetSize = numFunctions + numPositions + numValues
    val alphabetSize = 1000000000

    // Set up factory
    var factory: LibALFFactory = JNIFactory.STATIC
    var base: Knowledgebase = factory.createKnowledgebase()


    // Collect samples
    val functions = MutexFunction

    // Good
    print("Collecting Good Samples...")
    for(i <- 0 until 5) {
      file2database("good-mutex-" + i + ".txt", base, true, 20, functions)
    }
    println("OK")

    // Bad
    print("Collecting Bad Samples...")
    for(i <- 0 until 2) {
      file2database("bad-mutex-" + i + ".txt", base, false, 20, functions)
    }
    println("OK")

    // Run
    print("Setting Algorithm...")
    var algorithm = factory.createLearningAlgorithm(Algorithm.RPNI, base,
                                                    new java.lang.Integer(alphabetSize))
    println("OK")
    print("Generating Automaton...")
    var automaton = algorithm.advance().asInstanceOf[BasicAutomaton]
    println("OK")
    println()
    println(automaton.toDot())
    return automaton
  }

  def BasicTransition2Tuple(transition: de.libalf.BasicTransition): (Int, Int, Int) = {
    var s = transition.toString
    val numbers = s.split('(')(1).split(')')(0).split(", ")
    return (numbers(0).toInt, numbers(1).toInt, numbers(2).toInt)
  }

  def generateCode(automaton: BasicAutomaton, functions: CodeFunction): String= {
    val stateCount = automaton.getAlphabetSize
    val startState = automaton.getInitialStates.iterator.next
    var s = """
    class DFAStepper {
    private:
      int dfaState;
    public:
    """;

    for(state <- 0 to stateCount) {
      val methodName = word2line(state, functions)
      if(methodName == "Mutex") {
        s += """
        DFAStepper();
        """
      } else if(methodName == "~Mutex") {
        s += """
        ~DFAStepper();
        """
      } else {
        s += """
        void """ + methodName + """();
        """
      }
    }
    s += """
    };
    """

    s += """
    // Begin DFAStepper
    """
    for(state <- 0 to stateCount) {
      val methodName = word2line(state, functions)

      if(methodName == "Mutex") {
        s += """
        DFAStepper::DFAStepper() {
        // DFA Step Start
        """
      } else if(methodName == "~Mutex") {
        s += """
        DFAStepper::~DFAStepper() {
        // DFA Step Start
        """
      } else {
        s += """
        void DFAStepper::""" + methodName + """() {
        // DFA Step Start
        """
      }

      if(methodName == "Mutex") {
        s += """
        dfaState = """ + startState + """;
        """
      }

      var firstInChain = true
      for(transitionNumber <- 0 until automaton.getTransitions.size) {
        val transition = BasicTransition2Tuple(automaton.getTransitions.get(transitionNumber))
        if(transition._2 == state) {
          if(firstInChain) {
            s += """
              if(this->dfaState == """ + transition._1 + """) {
                cout << "Changing state from: """ + transition._1 + """ to: """ + transition._3 + """ Recieved: """ + methodName + """ " << endl;
                this->dfaState = """ + transition._3 + """;
              }
            """
            firstInChain = false
          } else {
            s += """
              else if(this->dfaState == """ + transition._1 + """) {
                cout << "Changing state from: """ + transition._1 + """ to: """ + transition._3 + """ Recieved: """ + methodName + """ " << endl;
                this->dfaState = """ + transition._3 + """;
              }
            """
          }
        }
      }

      if(methodName == "~Mutex") {
        s += """
        int goodRun = 0;
        """;
        var finalStates = automaton.getFinalStates.toArray
        for(i <- 0 until finalStates.size) {
          s += """
          if(dfaState == """ + finalStates(i) + """) {
            goodRun = 1;
          }
          """
        }
        s += """
        if(goodRun) {
          std::cout << "PASS!!!" << std::endl;
        } else {
          std::cout << "ERROR!!! Invalid Run!!!" << std::endl;
        }
        """
      }

      s += """
      // DFA Step End
      }
      """
    }

    s += """
    // End DFAStepper
    """

    return s;
  }

  def generateDot(automaton: BasicAutomaton, functions: CodeFunction): String= {
    val startState = automaton.getInitialStates.iterator.next
    var s = """
    digraph finite_automaton {
    graph[fontsize=8]
    rankdir=LR;
    size=8;
    """;

    for(state <- 0 until automaton.getNumberOfStates) {
      var finalStates = automaton.getFinalStates.toArray
      var finalState = false
      for(i <- 0 until finalStates.size) {
        if(state == finalStates(i)) {
          finalState = true
        }
      }
      if(finalState) {
        s += """
        node [shape=doublecircle, style="", color=black]; q""" + state + """;
        """
      } else {
        s += """
        node [shape=circle, style="", color=black]; q""" + state + """;
        """
      }
    }
    s += """
    node [shape=plaintext, label="", style=""]; iq0;
    """

    for(transitionNumber <- 0 until automaton.getTransitions.size) {
      val transition = BasicTransition2Tuple(automaton.getTransitions.get(transitionNumber))
      val methodName = word2line(transition._2, functions)
      s += """
      q""" + transition._1 + """ -> q""" + transition._3 + """ [label=" """ + methodName + """ "];
      """
    }

    s += """
    iq0 -> q""" + startState + """ [color=blue];
    }
    """
    return s;
  }

  def dotify(automaton: BasicAutomaton) {
    Some(new PrintWriter("out.dot")).foreach{p => p.write(automaton.toDot); p.close}
    //Process("dot", Seq("-Tpdf out.dot > out.pdf")).!!
  }

  var autamaton = generateAutomaton()
  var code = generateCode(autamaton, MutexFunction)
  println(code)
  Some(new PrintWriter("stepper.h")).foreach{p => p.write(code); p.close}
  dotify(autamaton)
  var nicerGraph = generateDot(autamaton, MutexFunction)
  println(nicerGraph)
  Some(new PrintWriter("out2.dot")).foreach{p => p.write(nicerGraph); p.close}
}
