package model

import logic.DFA
import definition.WLP4

class WLP4DFA extends DFA {
  val alphabet: Set[Char] = ".;$,()".toSet ++ ('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')
  val accepting: Set[String] = WLP4.tokens
  val states: Set[String] = accepting ++ Set("MINUS", "START")
  val start: String = "START"

  require(accepting subsetOf states)
  require(states contains start)

  override def transition: PartialFunction[(String, Char), String] = {
    case ("START",x)      if x.isLetter               => "ID"
    case ("ID",x)         if x.isLetterOrDigit        => "ID"
    case ("START", x)     if x.isDigit                => "NUM"
    case ("NUM", x)       if x.isDigit                => "NUM"
    case ("START",'-')                                => "MINUS"
    case ("MINUS",x)      if x.isDigit                => "NUM"
    case ("START", '}')                               => "RBRACE"
    case ("START", '{')                               => "LBRACE"
    case ("START", ']')                               => "RBRACK"
    case ("START", '[')                               => "LBRACK"
    case ("START", ')')                               => "RPAREN"
    case ("START", '(')                               => "LPAREN"
    case ("START",',')                                => "COMMA"
    case ("START",';')                                => "SEMI"
  }
}
