object CodeGen {
  private var labelNum = 0
  private var varMap = new collection.immutable.HashMap[String,Int]()
  case class CodeGenError(message: String) extends Exception(message)
  var output = ""

  def output(string: String): Unit = output += string

  def genProg(ast: Program): String = ast match {
    case Prog(Fun(id, blockItemList)) => genFunction(id,blockItemList)
  }
  private def genFunction(id:String, blockItems:List[BlockItem]) = {
    val stackIndex = -8
    varMap = new collection.immutable.HashMap[String,Int]()
    s".globl $id\n$id:\npush %rbp\nmov %rsp, %rbp\n"+genBlock(blockItems,varMap,stackIndex)
    //if no return in main function then return 0
    //if(id=="main") string += "mov $0, %eax\nret\n"
  }
  private def genDeclare(id:String,exp:Option[Exp],varMap:collection.immutable.HashMap[String,Int],stackIndex:Int,
                         currentScope:List[String])  = {
      if(currentScope.contains(id)) throw CodeGenError("Variable: "+id+" in this scope")
      else {val s =exp match{
        case None => "" case Some(e)=>genStatement(e,varMap,stackIndex)}
        (s+"push %rax\n",varMap + (id->stackIndex),stackIndex-8,id::currentScope)}
  }
  private def genBlock(blockItems: List[BlockItem], varMap:collection.immutable.HashMap[String,Int], stackIndex:Int):String= {
    var output = ""
    var currentScope:List[String] = List()
    var stackI:Int = stackIndex
    var varM:collection.immutable.HashMap[String,Int] = varMap
    for (blockItem <- blockItems) {
      blockItem match {
        case Declare(id, exp) => val result=genDeclare(id, exp, varM, stackI,currentScope)
          varM=result._2;stackI=result._3;currentScope=result._4;output+=result._1
        case _ if blockItem.isInstanceOf[Statement] => output+=genStatement(blockItem.asInstanceOf[Statement],varM,stackI)
      }
    }
    //pop items declared in this block
    val bytes_to_dealocate = 8*currentScope.length
    if(bytes_to_dealocate>0) output+=s"add $$$bytes_to_dealocate, %rsp\n"
    output
  }
  private def genCompare(flag:String, e1:String, e2:String) = e1 + "push %rax\n" + e2 +
    "pop %rcx\ncmp %rax, %rcx\nmov $0, %rax\nset"+flag+" %al\n"
  private def genCalculate(e1:String, e2:String) = e1 + "push %rax\n" + e2 + "pop %rcx\n"
  private def genBinOp(binOp: BinaryOp, e1:String, e2:String) : String = {
    binOp match {
      case OpAddition => genCalculate(e1,e2) + "add %rcx, %rax\n"
      case OpMultiplication => genCalculate(e1,e2) + "imul %rcx, %rax\n"
      //sub operands calculated in reverse order as 'subl e1 e2' does e2=e2-e1
      case OpSubtraction => genCalculate(e2,e1) + "sub %rcx, %rax\n"
      case OpDivision => genCalculate(e2,e1) + "mov $0, %rdx\nidiv %rcx\n"
      case OpModulo => genCalculate(e2,e1) + "mov $0, %rdx\nidiv %rcx\nmov %rdx, %rax\n"
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
      case OpBitwiseAnd => genCalculate(e1,e2) + "and %rcx, %rax\n"
      case OpBitwiseXor => genCalculate(e1,e2) + "xor %rcx, %rax\n"
      case OpBitwiseOr => genCalculate(e1,e2) + "or %rcx, %rax\n"
      case OpShiftLeft => genCalculate(e2,e1) + "shl %cl, %rax\n"
      case OpShiftRight => genCalculate(e2,e1) + "shr %cl, %rax\n"
    }
  }
  private def genStatement(ast: Statement,varMap:collection.immutable.HashMap[String,Int],stackIndex:Int): String = ast match {
    case Assign(id, exp) => if(varMap.contains(id)) {val offset:Int = varMap(id);genStatement(exp,varMap,stackIndex)+
      s"mov %rax, $offset(%rbp)\n"} else throw CodeGenError("Variable: "+id+" not defined before assignment")
    case Var(id) => if(varMap.contains(id)) {val offset = varMap(id);s"mov $offset(%rbp), %rax\n"} else
      throw CodeGenError("Variable: "+id+" not defined before reference")
    case Const(i) => s"mov $$$i, %rax\n"
    case Return(exp) => genStatement(exp,varMap,stackIndex)+"mov %rbp, %rsp\npop %rbp\nret\n"
    case UnOp(OpNegation, exp) => genStatement(exp,varMap,stackIndex) + "neg %rax\n"
    case UnOp(OpBitwiseComp, exp) => genStatement(exp,varMap,stackIndex) + "not %rax\n"
    case UnOp(OpLogicalNeg, exp) => genStatement(exp,varMap,stackIndex) + "cmp $0, %rax\nmov $0, %rax\nsete %al\n"
    case BinOp(binaryOp, term, next_term) => genBinOp(binaryOp, genStatement(term,varMap,stackIndex), genStatement(next_term,varMap,stackIndex))
    case Conditional(e1,e2,e3) => val l1=genLabel;val l2=genLabel;genStatement(e1,varMap,stackIndex)+"cmp $0, %rax\nje "+l1+"\n"+
      genStatement(e2,varMap,stackIndex)+"jmp "+l2+"\n"+l1+":\n"+genStatement(e3,varMap,stackIndex)+l2+":\n"
    case If(exp,s1,s2) => val l1=genLabel;genStatement(exp,varMap,stackIndex)+"cmp $0, %rax\nje "+l1+"\n"+genStatement(s1,varMap,stackIndex)+{s2 match {
      case None => l1+":\n" case Some(s)=>val l2=genLabel;"jmp "+l2+"\n"+l1+":\n"+genStatement(s,varMap,stackIndex)+l2+":\n"}}
    case Compound(blockItems:List[BlockItem]) => genBlock(blockItems,varMap,stackIndex)
  }
  private def genLabel = {labelNum+=1; "l"+labelNum.toString}
}
