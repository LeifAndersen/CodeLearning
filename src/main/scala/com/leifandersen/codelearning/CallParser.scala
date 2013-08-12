package com.leifandersen.codelearning

object CallParser extends RegexParser {

  def term : Parser[String] = "[^(),; ]".r ^^ _

  def expr : Parser[List[String]] = token ~ "(" ~ rep(token ~ ",") ~ ")" ~ ";" ^^ {
    case 
  }

  def apply(input: String): List[String] = parseAll(expr, input) match {
    case Success(result, _) => result
    case failure : NoSuccess => scala.sys.error(failure.msg)
  }
}
