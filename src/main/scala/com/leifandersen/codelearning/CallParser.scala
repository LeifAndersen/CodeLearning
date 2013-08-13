package com.leifandersen.codelearning

import scala.util.parsing.combinator
import scala.util.parsing.combinator._

object CallParser extends RegexParsers {

  def term : Parser[String] = "[a-zA-Z][a-zA-Z0-9]*".r

  def arguement : Parser[String] = "," ~ term ^^ {
    case _ ~ term => term
  }

  def arguements : Parser[List[String]] = term ~ rep(arguement) ^^ {
    case term ~ arguements => term +: arguements

  }

  def expr : Parser[List[String]] = term ~ "(" ~ opt(arguements) ~ ")" ~ ";" ^^ {
    case term ~ _ ~ arguements ~ _ ~ _ => {
      if(arguements.isEmpty) {
        List(term)
      } else {
        term +: arguements.orNull
      }
    }
  }

  def apply(input: String): List[String] = parseAll(expr, input) match {
    case Success(result, _) => result
    case failure : NoSuccess => scala.sys.error(failure.msg)
  }
}
