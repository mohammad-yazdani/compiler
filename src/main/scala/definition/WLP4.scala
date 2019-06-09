package definition

object WLP4 {

  val tokens: Set[String] = Set("ID", "NUM", "LPAREN", "RPAREN","LBRACE","RBRACE","RETURN","IF",
    "ELSE","WHILE","PRINTLN","WAIN","BECOMES","INT","EQ","NE","LT","GT","LE","GE","PLUS","MINUS",
    "STAR","SLASH","PCT","COMMA","SEMI","NEW","DELETE","LBRACK","RBRACK","AMP","NULL")

  val whiteSpace: Set[String] = Set("SPACE", "TAB", "NEWLINE", "COMMENT")
}
