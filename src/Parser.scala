
abstract class AstNode

abstract class Exp extends AstNode
case class BinOp(binaryOp: BinaryOp, exp: Exp, next_exp: Exp) extends Exp {
  override def toString: String = "("+exp.toString+binaryOp.toString+next_exp.toString+")"
}
case class UnOp(unaryOp: UnaryOp, exp: Exp) extends Exp {
  override def toString: String = "("+unaryOp.toString+exp.toString+")"
}
case class Const(int: Int) extends Exp {
  //override def toString: String = "Int<"+int+">"
  override def toString: String = int.toString
}

abstract class Statement extends AstNode
case class Return(exp: Exp) extends Statement {
  override def toString: String = "RETURN " + exp.toString
}

abstract class FunDecl extends AstNode
case class Fun(id: String, statement: Statement) extends FunDecl {
  override def toString: String = "FUN " + id + ":\n" + statement.toString
}

abstract class Program extends AstNode
case class Prog(funDecl: FunDecl) extends Program {
  override def toString: String = funDecl.toString
}

abstract class UnaryOp extends AstNode
case object OpNegation extends UnaryOp {
  override def toString: String = "-"
}
case object OpBitwiseComp extends UnaryOp
{
  override def toString: String = "~"
}
case object OpLogicalNeg extends UnaryOp
{
  override def toString: String = "!"
}

abstract class BinaryOp extends AstNode
case object OpAddition extends BinaryOp{
  override def toString: String = "+"
}
case object OpSubtraction extends BinaryOp{
  override def toString: String = "-"
}
case object OpMultiplication extends BinaryOp{
  override def toString: String = "*"
}
case object OpDivision extends BinaryOp{
  override def toString: String = "/"
}
case object OpAnd extends BinaryOp{
  override def toString: String = "&&"
}
case object OpOr extends BinaryOp{
  override def toString: String = "||"
}
case object OpEqual extends BinaryOp{
  override def toString: String = "=="
}
case object OpNotEqual extends BinaryOp{
  override def toString: String = "!="
}
case object OpLessThan extends BinaryOp{
  override def toString: String = "<"
}
case object OpLessThanOrEqual extends BinaryOp{
  override def toString: String = "<="
}
case object OpGreaterThan extends BinaryOp{
  override def toString: String = ">"
}
case object OpGreaterThanOrEqual extends BinaryOp{
  override def toString: String = ">="
}

case object OpModulo extends BinaryOp{
  override def toString: String = "%"
}
case object OpBitwiseAnd extends BinaryOp{
  override def toString: String = "&"
}
case object OpBitwiseXor extends BinaryOp{
  override def toString: String = "^"
}
case object OpBitwiseOr extends BinaryOp{
  override def toString: String = "|"
}
case object OpShiftLeft extends BinaryOp{
  override def toString: String = "<<"
}
case object OpShiftRight extends BinaryOp{
  override def toString: String = ">>"
}

case class ParseError(message: String) extends Exception(message)

class Parser(buffer: collection.BufferedIterator[Token]) {
  def parseErrorMessage(expectedToken: Token, actualToken: Token): String = "expecting token "+expectedToken+
    " found token "+actualToken
  def parseErrorMessage(expectedAst: String, actualToken: Token) : String = "expecting node of Ast type  "+expectedAst+
    " found token "+actualToken
  private def advance = buffer next
  private def lookAhead = buffer head
  private def eat(t:Token): Unit = {
    val token = advance
    if(t != token)
      throw ParseError(parseErrorMessage(t, token))
  }
  private def getUnOp(token:Token) : UnaryOp = token match {
    case Minus => OpNegation
    case BitwiseComp => OpBitwiseComp
    case LogicalNeg => OpLogicalNeg
  }
  private def getBinOp(token: Token): BinaryOp = token match {
    case Plus => OpAddition
    case Minus => OpSubtraction
    case Star => OpMultiplication
    case Slash => OpDivision
    case Percentage => OpModulo
    case DoubleAnd => OpAnd
    case DoubleOr => OpOr
    case And => OpBitwiseAnd
    case Or => OpBitwiseOr
    case Xor => OpBitwiseXor
    case DoubleEqual => OpEqual
    case NotEqual => OpNotEqual
    case LessThan => OpLessThan
    case LessThanOrEqual => OpLessThanOrEqual
    case GreaterThan => OpGreaterThan
    case GreaterThanOrEqual => OpGreaterThanOrEqual
    case DoubleLessThan => OpShiftLeft
    case DoubleGreaterThan => OpShiftRight
  }
  private def parseExp : Exp = {
    var exp = parseLogicalAndExpr
    while(lookAhead == DoubleOr) {
      val op = getBinOp(advance)
      val next_exp = parseLogicalAndExpr
      exp = BinOp(op, exp, next_exp)
    }
    exp
  }
  private def parseLogicalAndExpr : Exp = {
    var exp = parseBitwiseOr
    while(lookAhead == DoubleAnd) {
      val op = getBinOp(advance)
      val next_exp = parseBitwiseOr
      exp = BinOp(op, exp, next_exp)
    }
    exp
  }
  private def parseBitwiseOr : Exp = {
    var exp = parseBitwiseXor
    while(lookAhead == Or) {
      val op = getBinOp(advance)
      val next_exp = parseBitwiseXor
      exp = BinOp(op, exp, next_exp)
    }
    exp
  }
  private def parseBitwiseXor : Exp = {
    var exp = parseBitwiseAnd
    while(lookAhead == Xor) {
      val op = getBinOp(advance)
      val next_exp = parseBitwiseAnd
      exp = BinOp(op, exp, next_exp)
    }
    exp
  }
  private def parseBitwiseAnd : Exp = {
    var exp = parseEqualityExp
    while(lookAhead == And) {
      val op = getBinOp(advance)
      val next_exp = parseEqualityExp
      exp = BinOp(op, exp, next_exp)
    }
    exp
  }
  private def parseEqualityExp : Exp = {
    var exp = parseRelationalExp
    while(lookAhead == DoubleEqual || lookAhead == NotEqual) {
      val op = getBinOp(advance)
      val next_exp = parseRelationalExp
      exp = BinOp(op, exp, next_exp)
    }
    exp
  }
  private def parseRelationalExp : Exp = {
    var exp = parseBitwiseShift
    while(lookAhead == GreaterThan || lookAhead == LessThan ||
      lookAhead == GreaterThanOrEqual || lookAhead == LessThanOrEqual) {
      val op = getBinOp(advance)
      val next_exp = parseBitwiseShift
      exp = BinOp(op, exp, next_exp)
    }
    exp
  }
  private def parseBitwiseShift : Exp = {
    var exp:Exp = parseAdditiveExp
    while(lookAhead  == DoubleLessThan || lookAhead == DoubleGreaterThan) {
      val op = getBinOp(advance)
      val next_exp:Exp = parseAdditiveExp
      exp = BinOp(op, exp, next_exp)
    }
    exp
  }
  private def parseAdditiveExp : Exp = {
    var term:Exp = parseTerm
    while(lookAhead  == Plus || lookAhead == Minus) {
      val op = getBinOp(advance)
      val next_term:Exp = parseTerm
      term = BinOp(op, term, next_term)
    }
    term
  }
  private def parseTerm : Exp = {
    var factor:Exp = parseFactor
    while(lookAhead  == Star || lookAhead == Slash || lookAhead == Percentage) {
      val op = getBinOp(advance)
      val next_factor = parseFactor
      factor = BinOp(op, factor, next_factor)
    }
    factor
  }
  private def parseFactor: Exp = advance match {
    case OpenParenthesis => val exp = parseExp; eat(CloseParenthesis); exp
    case op if op == Minus || op == LogicalNeg || op == BitwiseComp => UnOp(getUnOp(op), parseFactor)
    case IntegerLiteral(n) => Const(n)
    case t => throw ParseError(parseErrorMessage("Factor", t))
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
