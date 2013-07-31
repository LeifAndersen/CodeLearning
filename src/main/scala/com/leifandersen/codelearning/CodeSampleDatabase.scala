package com.leifandersen.codelearning

class CodeSampleDatabase extends Iterable[(Long, List[String])] {
  var samples = Map[Long, List[String]]()
  var name = "__UNAMED__";

  def extend(id: Long, line: String) {
    if(samples.contains(id)) {
      samples += (id -> (samples(id) ++ List(line)))
    } else {
      samples += (id -> List(line))
    }
  }

  def extendList(id: Long, line: List[String]) {
    if(samples.contains(id)) {
      samples += (id -> (samples(id) ++ line))
    } else {
      samples += (id -> line)
    }
  }

  def lookup(id: Long): List[String] = {
    return samples(id)
  }

  def extendRaw(rawLine: String) {
    var words = rawLine.split(":")
    var sampleName = words(1)
    var databaseNumber = words(2)
    var line = words(3)
    extend(toInteger(databaseNumber), line)
    name = sampleName;
  }

  def extendRawList(lines: List[String]) {
    for(s <- lines) {
      extendRaw(s)
    }
  }

  def toInteger(s: String) = {
    val Hex = "(0x)?([0-9a-fA-F]+)".r
    s match {
      case Hex(_,s) => java.lang.Long.parseLong(s,16)
      case _ => java.lang.Long.parseLong(s, 10)
    }
  }

  def iterator = samples.iterator

}

object CodeSampleDatabase {
  def apply() = new CodeSampleDatabase()
}
