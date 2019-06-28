object CodeGen {
  def genCode(ast: AstNode): String = ast match {
    case Prog(Fun(string, Return(Const(i)))) => s".globl $string\n$string:\nmovl $$$i, %eax\nret\n"
  }
}
