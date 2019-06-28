import scala.annotation.tailrec

abstract class Token
case object OpenBrace extends Token
case object CloseBrace extends Token
case object OpenParenthesis extends Token
case object CloseParenthesis extends Token
case object Semicolon extends Token
case object EOF extends Token
case class Keyword(s: String) extends Token
case class Identifier(s: String) extends Token
case class IntegerLiteral(v : Int) extends Token


class Lexer(buffer: collection.BufferedIterator[Char]) {
  private val keyWords = List("int", "return")
  private def advance: Char = buffer next
  private def lookAhead: Option[Char] = if (buffer hasNext) Some(buffer head) else None
  @tailrec
  private def parseNumber(s: String): Token = lookAhead match{
    case None => IntegerLiteral(s.toInt)
    case Some(c) => c match {
      case a if a isDigit => advance; parseNumber(s+c)
      case _ => IntegerLiteral(s.toInt)
    }
  }
  @tailrec
  private def parseIdentifier(s: String): Token = lookAhead match {
    case None => if (keyWords contains s) Keyword(s) else Identifier(s)
    case Some(c) => c match {
      case a if a.toString matches "\\w" => advance; parseIdentifier(s + a)
      case _ => if (keyWords contains s) Keyword(s) else Identifier(s)
    }
  }
  @tailrec
  final def getToken : Option[Token] = lookAhead match {
    case None => None
    case Some(c) => c match {
        //skip whitespace
      case a if a.toString matches "\\s" => advance; getToken
      case a if a isDigit => Some(parseNumber(""))
      case a if a.toString matches "[_a-zA-Z]" => Some(parseIdentifier(""))
      case '{' => advance; Some(OpenBrace)
      case '}' => advance; Some(CloseBrace)
      case '(' => advance; Some(OpenParenthesis)
      case ')' => advance; Some(CloseParenthesis)
      case ';' => advance; Some(Semicolon)
    }
  }
}