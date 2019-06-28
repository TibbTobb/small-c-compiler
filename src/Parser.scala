abstract class AstNode

abstract class Exp extends AstNode
case class Const(int: Int) extends Exp

abstract class Statement extends AstNode
case class Return(exp: Exp) extends Statement

abstract class FunDecl extends AstNode
case class Fun(string: String, statement: Statement) extends FunDecl

abstract class Program extends AstNode
case class Prog(funDecl: FunDecl) extends Program

case class ParseError(message: String) extends Exception(message)

class Parser(buffer: collection.Iterator[Token]) {
  def parseErrorMessage(expectedToken : Token, actualToken:Token) : String = "expecting token "+expectedToken+
    " found token "+actualToken
  private def advance = buffer next
  private def eat(t:Token): Unit = {
    val token = advance
    if(t != token)
      throw ParseError(parseErrorMessage(t, token))
  }
  private def parseExp : Exp = advance match {
    case IntegerLiteral(a) => Const(a)
    case t => throw ParseError(parseErrorMessage(IntegerLiteral(0),t))
  }
  private def parseStatment : Statement = {
    eat(Keyword("return"))
    val exp = parseExp
    eat(Semicolon)
    Return(exp)
  }
  private def parseFunDecl : FunDecl = {
    eat(Keyword("int"))
    val id = parseId
    eat(OpenParenthesis)
    eat(CloseParenthesis)
    eat(OpenBrace)
    val statement = parseStatment
    eat(CloseBrace)
    Fun(id, statement)
  }
  def parseProgram : Program = Prog(parseFunDecl)

  private def parseId : String = advance match {
    case Identifier(s) => s
    case t => throw ParseError(parseErrorMessage(Identifier(""), t))
  }
}
