import scala.collection.mutable.ArrayBuffer

abstract class AstNode

abstract class Program extends AstNode

case class Prog(funDeclList: List[FunDecl]) extends Program {
  override def toString: String = funDeclList.toString
}

abstract class FunDecl extends AstNode

case class Fun(name: String, parameters:List[String], blockItemList: List[BlockItem]) extends FunDecl {
  override def toString: String = "FUN " + name + ":\n"+"Parameters:"+parameters.toString()+"\n"+ blockItemList.toString+"\n"
}

abstract class BlockItem extends AstNode

case class Declare(id: String, exp: Option[Exp]) extends BlockItem


abstract class Statement extends BlockItem

case class Return(exp: Exp) extends Statement {
  override def toString: String = "RETURN " + exp.toString
}

case class If(exp: Exp, statement: Statement, elseStatement: Option[Statement]) extends Statement

case class Compound(blockItems: List[BlockItem]) extends Statement

case class Expression(expOption: Option[Exp]) extends Statement

case class For(initial: Option[Exp], condition: Exp, post: Option[Exp], body: Statement) extends Statement

case class ForDecl(initial: Declare, condition: Exp, post: Option[Exp], body: Statement) extends Statement

case class While(condition: Exp, body: Statement) extends Statement

case class Do(body: Statement, condition: Exp) extends Statement

case class Break() extends Statement

case class Continue() extends Statement


abstract class Exp extends AstNode

case class FunCall(name:String,arguments:List[Exp]) extends Exp

case class Conditional(cond: Exp, ifExp: Exp, elseExp: Exp) extends Exp

case class Assign(id: String, exp: Exp) extends Exp

case class Var(id: String) extends Exp

case class UnOp(unaryOp: UnaryOp, exp: Exp) extends Exp {
  override def toString: String = "(" + unaryOp.toString + exp.toString + ")"
}

case class Const(int: Int) extends Exp {
  //override def toString: String = "Int<"+int+">"
  override def toString: String = int.toString
}

case class BinOp(binaryOp: BinaryOp, exp: Exp, next_exp: Exp) extends Exp {
  override def toString: String = "(" + exp.toString + binaryOp.toString + next_exp.toString + ")"
}

abstract class UnaryOp extends AstNode

case object OpNegation extends UnaryOp {
  override def toString: String = "-"
}

case object OpBitwiseComp extends UnaryOp {
  override def toString: String = "~"
}

case object OpLogicalNeg extends UnaryOp {
  override def toString: String = "!"
}

abstract class BinaryOp extends AstNode

case object OpAddition extends BinaryOp {
  override def toString: String = "+"
}

case object OpSubtraction extends BinaryOp {
  override def toString: String = "-"
}

case object OpMultiplication extends BinaryOp {
  override def toString: String = "*"
}

case object OpDivision extends BinaryOp {
  override def toString: String = "/"
}

case object OpAnd extends BinaryOp {
  override def toString: String = "&&"
}

case object OpOr extends BinaryOp {
  override def toString: String = "||"
}

case object OpEqual extends BinaryOp {
  override def toString: String = "=="
}

case object OpNotEqual extends BinaryOp {
  override def toString: String = "!="
}

case object OpLessThan extends BinaryOp {
  override def toString: String = "<"
}

case object OpLessThanOrEqual extends BinaryOp {
  override def toString: String = "<="
}

case object OpGreaterThan extends BinaryOp {
  override def toString: String = ">"
}

case object OpGreaterThanOrEqual extends BinaryOp {
  override def toString: String = ">="
}

case object OpModulo extends BinaryOp {
  override def toString: String = "%"
}

case object OpBitwiseAnd extends BinaryOp {
  override def toString: String = "&"
}

case object OpBitwiseXor extends BinaryOp {
  override def toString: String = "^"
}

case object OpBitwiseOr extends BinaryOp {
  override def toString: String = "|"
}

case object OpShiftLeft extends BinaryOp {
  override def toString: String = "<<"
}

case object OpShiftRight extends BinaryOp {
  override def toString: String = ">>"
}

case class ParseError(message: String) extends Exception(message)

class Parser(tokens: List[Token]) {
  var position = 0

  def parseErrorMessage(expectedToken: Token, actualToken: Token): String = "expecting token " + expectedToken +
    " found token " + actualToken + " at position " + position

  def parseErrorMessage(expectedAst: String, actualToken: Token): String = "expecting node of Ast type  " + expectedAst +
    " found token " + actualToken + " at position " + position

  private def advance: Token = {
    position += 1; tokens(position - 1)
  }

  private def lookAhead: Token = tokens(position)

  private def lookAhead2Option = if (tokens.length > (position + 1)) Some(tokens(position + 1)) else None

  private def eat(t: Token): Unit = {
    val token = advance
    if (t != token)
      throw ParseError(parseErrorMessage(t, token))
  }

  private def getUnOp(token: Token): UnaryOp = token match {
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

  private def parseExpOption: Option[Exp] = lookAhead match {
    case Semicolon | CloseParenthesis=> None
    case _ => Some(parseExp)
  }

  private def parseExp: Exp = lookAhead match {
    case Identifier(id) if lookAhead2Option.contains(Equal) =>
      advance; advance
      val e = parseExp; Assign(id, e)
    case _ => parseConditionalExpr
  }

  private def parseConditionalExpr: Exp = {
    val exp = parseLogicalOrExpr
    if (lookAhead == QuestionMark) {
      advance
      val ifExp = parseExp
      eat(Colon)
      val elseExp = parseConditionalExpr
      Conditional(exp, ifExp, elseExp)
    } else {
      exp
    }
  }

  private def parseLogicalOrExpr: Exp = {
    var exp = parseLogicalAndExpr
    while (lookAhead == DoubleOr) {
      val op = getBinOp(advance)
      val next_exp = parseLogicalAndExpr
      exp = BinOp(op, exp, next_exp)
    }
    exp
  }

  private def parseLogicalAndExpr: Exp = {
    var exp = parseBitwiseOr
    while (lookAhead == DoubleAnd) {
      val op = getBinOp(advance)
      val next_exp = parseBitwiseOr
      exp = BinOp(op, exp, next_exp)
    }
    exp
  }

  private def parseBitwiseOr: Exp = {
    var exp = parseBitwiseXor
    while (lookAhead == Or) {
      val op = getBinOp(advance)
      val next_exp = parseBitwiseXor
      exp = BinOp(op, exp, next_exp)
    }
    exp
  }

  private def parseBitwiseXor: Exp = {
    var exp = parseBitwiseAnd
    while (lookAhead == Xor) {
      val op = getBinOp(advance)
      val next_exp = parseBitwiseAnd
      exp = BinOp(op, exp, next_exp)
    }
    exp
  }

  private def parseBitwiseAnd: Exp = {
    var exp = parseEqualityExp
    while (lookAhead == And) {
      val op = getBinOp(advance)
      val next_exp = parseEqualityExp
      exp = BinOp(op, exp, next_exp)
    }
    exp
  }

  private def parseEqualityExp: Exp = {
    var exp = parseRelationalExp
    while (lookAhead == DoubleEqual || lookAhead == NotEqual) {
      val op = getBinOp(advance)
      val next_exp = parseRelationalExp
      exp = BinOp(op, exp, next_exp)
    }
    exp
  }

  private def parseRelationalExp: Exp = {
    var exp = parseBitwiseShift
    while (lookAhead == GreaterThan || lookAhead == LessThan ||
      lookAhead == GreaterThanOrEqual || lookAhead == LessThanOrEqual) {
      val op = getBinOp(advance)
      val next_exp = parseBitwiseShift
      exp = BinOp(op, exp, next_exp)
    }
    exp
  }

  private def parseBitwiseShift: Exp = {
    var exp: Exp = parseAdditiveExp
    while (lookAhead == DoubleLessThan || lookAhead == DoubleGreaterThan) {
      val op = getBinOp(advance)
      val next_exp: Exp = parseAdditiveExp
      exp = BinOp(op, exp, next_exp)
    }
    exp
  }

  private def parseAdditiveExp: Exp = {
    var term: Exp = parseTerm
    while (lookAhead == Plus || lookAhead == Minus) {
      val op = getBinOp(advance)
      val next_term: Exp = parseTerm
      term = BinOp(op, term, next_term)
    }
    term
  }

  private def parseTerm: Exp = {
    var factor: Exp = parseFactor
    while (lookAhead == Star || lookAhead == Slash || lookAhead == Percentage) {
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
    case Identifier(s) => lookAhead match {
      case OpenParenthesis => advance
        var expressions = new ArrayBuffer[Exp]
        var first = true
        while (lookAhead != CloseParenthesis) {
          if (!first) eat(Comma)
          expressions.append(parseExp)
          first = false
        }
        advance
        FunCall(s, expressions.toList)
      case _ => Var(s)
    }
    case t => throw ParseError(parseErrorMessage("Factor", t))
  }

  private def parseStatement: Statement = lookAhead match {
    case Keyword("return") => advance; val exp = parseExp; eat(Semicolon); Return(exp)
    case Keyword("if") => advance
      eat(OpenParenthesis)
      val e = parseExp
      eat(CloseParenthesis)
      val s = parseStatement
      var s2: Option[Statement] = None
      if (lookAhead == Keyword("else")) {
        advance
        s2 = Some(parseStatement)
      }
      If(e, s, s2)

    case Keyword("for") => advance
      eat(OpenParenthesis)
      val e1Either: Either[Option[Exp], Declare] = lookAhead match {
        case Keyword("int") => Right(parseDeclare)
        case _ => val e1 = parseExpOption; eat(Semicolon); Left(e1)
      }
      //replace empty condition with non-zero value
      val e2 = parseExpOption match {
        case None => Const(1)
        case Some(e) => e
      }
      eat(Semicolon)
      val e3 = parseExpOption
      eat(CloseParenthesis)
      val body = parseStatement
      e1Either match {
        case Left(e1) => For(e1, e2, e3, body)
        case Right(e1) => ForDecl(e1, e2, e3, body)
      }
    case Keyword("while") => advance
      eat(OpenParenthesis)
      val e = parseExp
      eat(CloseParenthesis)
      While(e, parseStatement)

    case Keyword("do") => advance
      val body = parseStatement
      eat(Keyword("while"))
      val e = parseExp
      eat(Semicolon)
      Do(body, e)
    case Keyword("break") => advance; eat(Semicolon); Break()
    case Keyword("continue") => advance; eat(Semicolon); Continue()

    case OpenBrace => advance
      val blockItems = new ArrayBuffer[BlockItem]
      while (lookAhead != CloseBrace) {
        blockItems.append(parseBlockItem)
      }
      advance
      Compound(blockItems.toList)
    case _ => val expOption = parseExpOption; eat(Semicolon); Expression(expOption)
  }

  private def parseDeclare: Declare = {
    eat(Keyword("int"))
    val id = parseId
    var exp: Option[Exp] = None
    if (lookAhead == Equal) {
      advance
      exp = Some(parseExp)
    }
    eat(Semicolon)
    Declare(id, exp)
  }

  private def parseBlockItem: BlockItem = lookAhead match {
    case Keyword("int") => parseDeclare
    case _ => parseStatement
  }

  private def parseFunDecl: FunDecl = {
    eat(Keyword("int"))
    val id = parseId
    eat(OpenParenthesis)
    val parameters = new ArrayBuffer[String]
    var first = true
    while(lookAhead!=CloseParenthesis) {
      if (!first) eat(Comma)
      eat(Keyword("int"))
      parameters.append(parseId)
      first = false
    }
    advance
    val statementList = new ArrayBuffer[BlockItem]
    if(lookAhead == OpenBrace) {
      advance
      while (lookAhead != CloseBrace) {
        statementList.append(parseBlockItem)
      }
      advance
    } else eat(Semicolon)
    Fun(id, parameters.toList,statementList.toList)
  }

  def parseProgram: Program = {
    val funDeclList = new ArrayBuffer[FunDecl]
    while(position!=tokens.length) {
      funDeclList.append(parseFunDecl)
    }
    Prog(funDeclList.toList)
  }

  private def parseId: String = advance match {
    case Identifier(s) => s
    case t => throw ParseError(parseErrorMessage(Identifier(""), t))
  }
}
