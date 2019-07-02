object CodeGen {
  var labelNum = 0
  def genProg(ast: Program): String = ast match {
    case Prog(Fun(id, Return(expr))) => s".globl $id\n$id:\n"+genExpr(expr)+"ret\n"
  }
  private def genCompare(flag:String, e1:String, e2:String) = e1 + "push %rax\n" + e2 +
    "pop %rcx\ncmpl %eax, %ecx\nmovl $0, %eax\nset"+flag+" %al\n"
  private def genCaculate(e1:String,e2:String) = e1 + "push %rax\n" + e2 + "pop %rcx\n"
  private def genBinOp(binOp: BinaryOp, e1:String, e2:String) : String = {
    binOp match {
      case OpAddition => genCaculate(e1,e2) + "addl %ecx, %eax\n"
      case OpMultiplication => genCaculate(e1,e2) + "imul %ecx, %eax\n"
      //sub operands calculated in reverse order as 'subl e1 e2' does e2=e2-e1
      case OpSubtraction => genCaculate(e2,e1) + "subl %ecx, %eax\n"
      case OpDivision => genCaculate(e2,e1) + "movl $0, %edx\nidivl %ecx\n"
      case OpModulo => genCaculate(e2,e1) + "movl $0, %edx\nidivl %ecx\nmovl %edx, %eax\n"
      case OpEqual => genCompare("e", e1, e2)
      case OpNotEqual => genCompare("ne", e1, e2)
      case OpGreaterThan => genCompare("g", e1, e2)
      case OpLessThan => genCompare("l", e1, e2)
      case OpGreaterThanOrEqual => genCompare("ge", e1, e2)
      case OpLessThanOrEqual => genCompare("le", e1, e2)
      case OpOr => val c2=genLabel;val end=genLabel; e1+"cmpl $0, %eax\nje "+c2+"\nmovl $1, %eax\njmp "+end+"\n"+
        c2+":\n"+e2+"cmpl $0, %eax\nmovl $0, %eax\nsetne %al\n"+end+":\n"
      case OpAnd => val end=genLabel; e1+"cmpl $0, %eax\nje "+end+"\n"+
        e2+"cmpl $0, %eax\nmovl $0, %eax\nsetne %al\n"+end+":\n"
      case OpBitwiseAnd => genCaculate(e1,e2) + "and %ecx, %eax\n"
      case OpBitwiseXor => genCaculate(e1,e2) + "xor %ecx, %eax\n"
      case OpBitwiseOr => genCaculate(e1,e2) + "or %ecx, %eax\n"
      case OpShiftLeft => genCaculate(e2,e1) + "shl %cl, %eax\n"
      case OpShiftRight => genCaculate(e2,e1) + "shr %cl, %eax\n"
    }
  }
  private def genExpr(ast: Exp): String = ast match {
    case Const(i) => s"movl $$$i, %eax\n"
    case UnOp(OpNegation, exp) => genExpr(exp) + "neg %eax\n"
    case UnOp(OpBitwiseComp, exp) => genExpr(exp) + "not %eax\n"
    case UnOp(OpLogicalNeg, exp) => genExpr(exp) + "cmpl $0, %eax\nmovl $0, %eax\nsete %al\n"
    case BinOp(binaryOp, term, next_term) => genBinOp(binaryOp, genExpr(term), genExpr(next_term))
  }
  private def genLabel = {labelNum+=1; "l"+labelNum.toString}
}
