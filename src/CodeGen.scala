object CodeGen {
  private var labelNum = 0
  private var stackIndex = -8
  private def updateStackIndex(): Unit = stackIndex -= 8
  private var varMap = new collection.immutable.HashMap[String,Int]()
  case class CodeGenError(message: String) extends Exception(message)

  def genProg(ast: Program): String = ast match {
    case Prog(Fun(id, blockItemList)) => genFunction(id,blockItemList)
  }
  private def genFunction(id:String, BlockItemList:List[BlockItem]) = {
    stackIndex = -8
    var string = s".globl $id\n$id:\npush %rbp\nmov %rsp, %rbp\n"
    string = BlockItemList.map(e=>genBlockItem(e)).foldLeft(string)(_+_)
    //if no return in main function then return 0
    //if(id=="main") string += "mov $0, %eax\nret\n"
    string
  }
  private def genBlockItem(blockItem: BlockItem) = blockItem match {
    case Declare(id, exp) => if(varMap.contains(id)) throw CodeGenError("Variable: "+id+" is defined multiple times") else
    {val s =exp match{
      case None => "" case Some(e)=>genStatement(e)}; varMap = varMap + (id->stackIndex); updateStackIndex(); s+"push %rax\n"}
    case _ if blockItem.isInstanceOf[Statement] => genStatement(blockItem.asInstanceOf[Statement])
  }
  private def genCompare(flag:String, e1:String, e2:String) = e1 + "push %rax\n" + e2 +
    "pop %rcx\ncmp %rax, %rcx\nmov $0, %rax\nset"+flag+" %al\n"
  private def genCaculate(e1:String,e2:String) = e1 + "push %rax\n" + e2 + "pop %rcx\n"
  private def genBinOp(binOp: BinaryOp, e1:String, e2:String) : String = {
    binOp match {
      case OpAddition => genCaculate(e1,e2) + "add %rcx, %rax\n"
      case OpMultiplication => genCaculate(e1,e2) + "imul %rcx, %rax\n"
      //sub operands calculated in reverse order as 'subl e1 e2' does e2=e2-e1
      case OpSubtraction => genCaculate(e2,e1) + "sub %rcx, %rax\n"
      case OpDivision => genCaculate(e2,e1) + "mov $0, %rdx\nidiv %rcx\n"
      case OpModulo => genCaculate(e2,e1) + "mov $0, %rdx\nidiv %rcx\nmov %rdx, %rax\n"
      case OpEqual => genCompare("e", e1, e2)
      case OpNotEqual => genCompare("ne", e1, e2)
      case OpGreaterThan => genCompare("g", e1, e2)
      case OpLessThan => genCompare("l", e1, e2)
      case OpGreaterThanOrEqual => genCompare("ge", e1, e2)
      case OpLessThanOrEqual => genCompare("le", e1, e2)
      case OpOr => val c2=genLabel;val end=genLabel; e1+"cmp $0, %rax\nje "+c2+"\nmov $1, %rax\njmp "+end+"\n"+
        c2+":\n"+e2+"cmp $0, %rax\nmov $0, %rax\nsetne %al\n"+end+":\n"
      case OpAnd => val end=genLabel; e1+"cmp $0, %rax\nje "+end+"\n"+
        e2+"cmp $0, %rax\nmov $0, %rax\nsetne %al\n"+end+":\n"
      case OpBitwiseAnd => genCaculate(e1,e2) + "and %rcx, %rax\n"
      case OpBitwiseXor => genCaculate(e1,e2) + "xor %rcx, %rax\n"
      case OpBitwiseOr => genCaculate(e1,e2) + "or %rcx, %rax\n"
      case OpShiftLeft => genCaculate(e2,e1) + "shl %cl, %rax\n"
      case OpShiftRight => genCaculate(e2,e1) + "shr %cl, %rax\n"
    }
  }
  private def genStatement(ast: Statement): String = ast match {
    case Assign(id, exp) => if(varMap.contains(id)) {val offset:Int = varMap(id);genStatement(exp)+
      s"mov %rax, $offset(%rbp)\n"} else throw CodeGenError("Variable: "+id+" not defined before assignment")
    case Var(id) => if(varMap.contains(id)) {val offset = varMap(id);s"mov $offset(%rbp), %rax\n"} else throw CodeGenError("Variable: "+id+" not defined before reference")
    case Const(i) => s"mov $$$i, %rax\n"
    case Return(exp) => genStatement(exp)+"mov %rbp, %rsp\npop %rbp\nret\n"
    case UnOp(OpNegation, exp) => genStatement(exp) + "neg %rax\n"
    case UnOp(OpBitwiseComp, exp) => genStatement(exp) + "not %rax\n"
    case UnOp(OpLogicalNeg, exp) => genStatement(exp) + "cmp $0, %rax\nmov $0, %rax\nsete %al\n"
    case BinOp(binaryOp, term, next_term) => genBinOp(binaryOp, genStatement(term), genStatement(next_term))
    case Conditional(e1,e2,e3) => val l1=genLabel;val l2=genLabel;genStatement(e1)+"cmp $0, %rax\nje "+l1+"\n"+
      genStatement(e2)+"jmp "+l2+"\n"+l1+":\n"+genStatement(e3)+l2+":\n"
    case If(exp,s1,s2) => val l1=genLabel;genStatement(exp)+"cmp $0, %rax\nje "+l1+"\n"+genStatement(s1)+{s2 match {
      case None => l1+":\n" case Some(s)=>val l2=genLabel;"jmp "+l2+"\n"+l1+":\n"+genStatement(s)+l2+":\n"}}
  }
  private def genLabel = {labelNum+=1; "l"+labelNum.toString}
}
