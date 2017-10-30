package model

import logic.DFA
import definition.WLP4

class WLP4DFA extends DFA {
  val alphabet: Set[Char] = null
  val states: Set[String] = null
  val start: String = "start"
  val accepting: Set[String] = WLP4.tokens

  override def transition: PartialFunction[(String, Char), String] = {
    case ("START",x)      if x.isLetter               => "ID"
    case ("ID",x)         if x.isLetterOrDigit        => "ID"
    case ("START", x)     if x.isDigit                => "INT"
    case ("START",'-')                                => "MINUS"
    case ("MINUS",x)      if x.isDigit                => "INT"
    case ("START", '}')                               => "RBRAC"
    case ("START", '{')                               => "LBRAC"
    case ("START", ']')                               => "RBRACK"
    case ("START", '[')                               => "LBRACK"
    case ("START", ')')                               => "RPAREN"
    case ("START", '(')                               => "LPAREN"
    case ("START",',')                                => "COMMA"
    case ("START",';')                                => "SEMI"
  }
}
