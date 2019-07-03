case class Context(varMap:collection.immutable.HashMap[String,Int],stackIndex:Int,
                   breakLabel:Option[String],continueLabel:Option[String],
                   funMap:collection.immutable.HashMap[String,(Int,Boolean)])

object CodeGen {
  private var labelNum = 0
  case class CodeGenError(message: String) extends Exception(message)
  var output = ""

  def output(string: String): Unit = output += string

  def genProg(ast: Program): String = ast match {
    case Prog(funList: List[Fun]) =>
      var output = ""
      //map of function name to number of arguments and if declared
      var funMap = new collection.immutable.HashMap[String, (Int, Boolean)]
      for (fun <- funList) {
        val isDefinition = fun.blockItemList.nonEmpty
        funMap.get(fun.name) match {
          case Some((_, true)) if isDefinition => throw CodeGenError("Two definitions of function: " + fun.name)
          case Some((n, _)) => if (n != fun.parameters.length)
            throw CodeGenError("Two declarations of function " + fun.name + " with different numbers of parameters")
          case None => funMap += (fun.name -> (fun.parameters.length, isDefinition))
        }
        if(isDefinition) output += genFunction(fun,funMap)
      }
      output
  }

  private def genFunction(fun : Fun,funMap:collection.immutable.HashMap[String,(Int,Boolean)]):String = {
    val stackIndex = -8
    val id = fun.name
    val blockItems = fun.blockItemList
    var varMap = new collection.immutable.HashMap[String,Int]()
    var currentScope:List[String] = List()
    var paramOffset = 16
    for(param <- fun.parameters) {
      varMap = varMap + (param->paramOffset)
      currentScope = param::currentScope
      paramOffset += 8
    }
    s".globl $id\n"+
      s"$id:\n"+
      "push %rbp\n"+
      "mov %rsp, %rbp\n"+
      genBlock(blockItems,Context(varMap,stackIndex,None,None,funMap),currentScope:List[String])
    //if no return in main function then return 0
    //if(id=="main") string += "mov $0, %eax\nret\n"
  }
  private def genDeclare(id:String,exp:Option[Exp], context:Context,currentScope:List[String])  = {
      if(currentScope.contains(id)) throw CodeGenError("Variable: "+id+" in this scope")
      else {val s =exp match{
        case None => "" case Some(e)=>genExpr(e,context)}
        val stackIndex= context.stackIndex
        (s+"push %rax\n",
          Context(context.varMap + (id->stackIndex),
          context.stackIndex-8, context.breakLabel,context.continueLabel,context.funMap),id::currentScope)}
  }

  private def genBlock(blockItems: List[BlockItem], context:Context,currentScope:List[String]):String= {
    var output = ""
    var currentS:List[String] = currentScope
    var currentContext = context
    for (blockItem <- blockItems) {
      blockItem match {
        case Declare(id, exp) =>
          val result=genDeclare(id, exp, currentContext, currentS)
          currentS = result._3
          currentContext=result._2
          output+=result._1
        case _ if blockItem.isInstanceOf[Statement] =>
          output+=genStatement(blockItem.asInstanceOf[Statement],currentContext)
      }
    }
    //pop items declared in this block
    val bytes_to_deallocate = 8*currentS.length
    if(bytes_to_deallocate>0) output+=s"add $$$bytes_to_deallocate, %rsp\n"
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
      case OpSubtraction => genCalculate(e2,e1) +
        "sub %rcx, %rax\n"

      case OpDivision => genCalculate(e2,e1) +
        "mov $0, %rdx\nidiv %rcx\n"

      case OpModulo => genCalculate(e2,e1) +
        "mov $0, %rdx\n" +
        "idiv %rcx\n" +
        "mov %rdx, %rax\n"

      case OpEqual => genCompare("e", e1, e2)
      case OpNotEqual => genCompare("ne", e1, e2)
      case OpGreaterThan => genCompare("g", e1, e2)
      case OpLessThan => genCompare("l", e1, e2)
      case OpGreaterThanOrEqual => genCompare("ge", e1, e2)
      case OpLessThanOrEqual => genCompare("le", e1, e2)

      case OpOr => val c2=genLabel;val end=genLabel
        e1+"cmp $0, %rax\n" +
          "je "+c2+"\n" +
          "mov $1, %rax\n" +
          "jmp "+end+"\n"+
          c2+":\n"+
          e2+"cmp $0, %rax\n" +
          "mov $0, %rax\n" +
          "setne %al\n"+
          end+":\n"

      case OpAnd => val end=genLabel; e1+"cmp $0, %rax\nje "+end+"\n"+
        e2+"cmp $0, %rax\nmov $0, %rax\nsetne %al\n"+end+":\n"

      case OpBitwiseAnd => genCalculate(e1,e2) +
        "and %rcx, %rax\n"

      case OpBitwiseXor => genCalculate(e1,e2) +
        "xor %rcx, %rax\n"

      case OpBitwiseOr => genCalculate(e1,e2) +
        "or %rcx, %rax\n"

      case OpShiftLeft => genCalculate(e2,e1) +
        "shl %cl, %rax\n"

      case OpShiftRight => genCalculate(e2,e1) +
        "shr %cl, %rax\n"
    }
  }
  private def genExpr(ast: Exp,context: Context):String = ast match {
    case Assign(id, exp) =>
      if (context.varMap.contains(id)) {
        val offset: Int = context.varMap(id)
        genExpr(exp, context) +
        s"mov %rax, $offset(%rbp)\n"
    } else throw CodeGenError("Variable: " + id + " not defined before assignment")

    case Var(id) => if (context.varMap.contains(id)) {
      val offset = context.varMap(id)
      s"mov $offset(%rbp), %rax\n"
    } else
      throw CodeGenError("Variable: " + id + " not defined before reference")

    case Const(i) => s"mov $$$i, %rax\n"

    case UnOp(OpNegation, exp) =>
      genExpr(exp, context) +
      "neg %rax\n"

    case UnOp(OpBitwiseComp, exp) =>
      genExpr(exp, context) +
      "not %rax\n"

    case UnOp(OpLogicalNeg, exp) =>
      genExpr(exp, context) +
      "cmp $0, %rax\n" +
      "mov $0, %rax\n" +
      "sete %al\n"

    case BinOp(binaryOp, term, next_term) =>
      genBinOp(binaryOp, genExpr(term, context), genExpr(next_term, context))

    case Conditional(e1,e2,e3) =>
      val l1=genLabel;val l2=genLabel
      genExpr(e1,context)+"cmp $0, %rax\n" +
        "je "+l1+"\n"+
      genExpr(e2,context)+
        "jmp "+l2+"\n"+
        l1+":\n"+
        genExpr(e3,context)+
        l2+":\n"

    case FunCall(name,arguments) =>
      //push arguments to stack in reverse order
      context.funMap.get(name) match {
        case None => throw CodeGenError("Function: "+name+" not declared before called")
        case Some((n,_)) =>
          if(n!=arguments.length) throw CodeGenError("Function "+name+" called with wrong number of arguments")
      }
      var output = ""
      for(arg <-arguments.reverse) {
        output += genExpr(arg,context) +
          "push %rax\n"
      }
      val bytes_to_remove = 8*arguments.length
      output+
        s"call $name\n"+
        s"add $$$bytes_to_remove, %rsp\n"
  }
  private def genStatement(ast: Statement,context:Context): String = ast match {
    case Expression(exp) =>
      exp match {
        case None => ""
        case Some(e)=>genExpr(e,context)
      }
    case Return(exp) =>
      genExpr(exp,context)+
        "mov %rbp, %rsp\n" +
        "pop %rbp\n" +
        "ret\n"

    case If(exp, s1, s2) =>
      val l1 = genLabel
      genExpr(exp, context)+
        "cmp $0, %rax\n" +
        "je " + l1 + "\n"+
        genStatement(s1, context)+
        (s2 match {
          case None => l1 + ":\n"
          case Some(s) => val l2 = genLabel
            "jmp " + l2 + "\n" +
              l1 + ":\n" +
              genStatement(s, context) +
              l2 + ":\n"})

    case Compound(blockItems:List[BlockItem]) => genBlock(blockItems,context,List())

    case While(condition,body) =>
      val (l1,l2)=(genLabel,genLabel)
      l1+":\n"+
        genExpr(condition,context)+
        "cmp $0, %rax\n"+
        "je "+l2+"\n"+
        genStatement(body,Context(context.varMap,context.stackIndex,Some(l2),Some(l1),context.funMap))+
        "jmp "+l1+"\n"+
          l2+":\n"

    case Do(body,condition) => val (l1,l2)=(genLabel,genLabel)
      l1+":\n"+
      genStatement(body,Context(context.varMap,context.stackIndex,Some(l2),Some(l1),context.funMap))+
      genExpr(condition,context)+
      "cmp $0, %rax\n"+
      "jne "+l1+"\n"+
      l2+":\n"

      //TODO: create genFor to reduce duplicated code in For and ForDecl
    case For(initial,condition,post,body) =>
      val(l1,l2,l3)=(genLabel,genLabel,genLabel)
      (initial match {case None => "" case Some(e)=>genExpr(e,context)})+
        l1+":\n"+
        genExpr(condition,context)+
        "cmp $0, %rax\n"+
        "je "+l2+"\n"+
        genStatement(body,Context(context.varMap,context.stackIndex,Some(l2),Some(l3),context.funMap))+
        l3+":\n"+
        (post match{case None=> "" case Some(e)=>genExpr(e,context)})+
        "jmp "+l1+"\n"+
        l2+":\n"

    case ForDecl(init,condition,post,body) =>
      val (l1,l2,l3)=(genLabel,genLabel,genLabel)
      var newContext = context
      //gen init
      (init match{case Declare(id, exp)=>
        //header is in its own block
        val result=genDeclare(id,exp,newContext,List())
        newContext=result._2
        result._1})+
      //gen condition
        l1+":\n"+
        genExpr(condition,newContext)+
        "cmp $0, %rax\n"+
        "je "+l2+"\n"+
      //gen statement
        genStatement(body,Context(newContext.varMap,newContext.stackIndex,Some(l2),Some(l3),context.funMap))+
        l3+":\n"+
        (post match{case None=> "" case Some(e)=>genExpr(e,newContext)})+
        "jmp "+l1+"\n"+
        l2+":\n"+
      //deallocate variable in init block
        s"add $$8, %rsp\n"

    case Break() =>
      context.breakLabel match{
        case Some(l)=>"jmp "+l+"\n"
        case None=> throw CodeGenError("'break' outside of loop")}

    case Continue() => context.continueLabel match{
      case Some(l)=>"jmp "+l+"\n"
      case None=> throw CodeGenError("'continue' outside of loop")}
  }
  private def genLabel = {labelNum+=1; "l"+labelNum.toString}
}
