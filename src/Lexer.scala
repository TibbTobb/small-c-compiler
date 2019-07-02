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

case object Minus extends Token
case object BitwiseComp extends Token
case object LogicalNeg extends Token

case object Plus extends Token
case object Star extends Token
case object Slash extends Token

case object DoubleAnd extends Token
case object DoubleOr extends Token
case object DoubleEqual extends Token
case object NotEqual extends Token
case object LessThan extends Token
case object LessThanOrEqual extends Token
case object GreaterThan extends Token
case object GreaterThanOrEqual extends Token

case object Percentage extends Token
case object And extends Token
case object Or extends Token
case object Xor extends Token
case object DoubleLessThan extends Token
case object DoubleGreaterThan extends Token

case object Equal extends Token

case class LexError(message: String) extends Exception(message)

class Lexer(buffer: collection.BufferedIterator[Char]) {
  private val keyWords = List("int", "return")
  private def advance: Char = buffer next
  private def lookAhead: Option[Char] = buffer headOption
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
      case _ if c.toString matches "\\s" => advance; getToken
      case _ if c isDigit => Some(parseNumber(""))
      case _ if c.toString matches "[_a-zA-Z]" => Some(parseIdentifier(""))
      case '{' => advance; Some(OpenBrace)
      case '}' => advance; Some(CloseBrace)
      case '(' => advance; Some(OpenParenthesis)
      case ')' => advance; Some(CloseParenthesis)
      case ';' => advance; Some(Semicolon)
      case '-' => advance; Some(Minus)
      case '~' => advance; Some(BitwiseComp)
      case '!' => advance; lookAhead match {
        case Some('=') => advance; Some(NotEqual)
        case _ => Some(LogicalNeg)
      }
      case '+' => advance; Some(Plus)
      case '*' => advance; Some(Star)
      case '/' => advance; Some(Slash)
      case '&' => advance; lookAhead match {
        case Some('&') => advance;Some(DoubleAnd)
        case _ => Some(And)
      }
      case '|' => advance; lookAhead match {
        case Some('|') => advance;Some(DoubleOr)
        case _ => Some(Or)
      }
      case '=' => advance; lookAhead match {
        case Some('=') => advance;Some(DoubleEqual)
        case _ => Some(Equal)
      }
      case '<' => advance; lookAhead match {
        case Some('=') => advance; Some(LessThanOrEqual)
        case Some('<') => advance; Some(DoubleLessThan)
        case _ => Some(LessThan)
      }
      case '>' => advance; lookAhead match {
        case Some('=') => advance; Some(GreaterThanOrEqual)
        case Some('>') => advance; Some(DoubleGreaterThan)
        case _ => Some(GreaterThan)
      }
      case '%' => advance; Some(Percentage)
      case '^' => advance; Some(Xor)
      case _ => throw LexError("Invalid character: "+c)
    }
  }
}