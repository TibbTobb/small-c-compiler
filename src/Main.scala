import java.io.{BufferedWriter, File, FileWriter}

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object Main {
  def main(args: Array[String]): Unit = {
    val fileName: String = args(0)
    val inputStream = io.Source.fromFile(fileName).buffered
    val lexer = new Lexer(inputStream)
    @tailrec
    def createTokenList( tokens : ArrayBuffer[Token]): ArrayBuffer[Token] = {
      lexer.getToken match {
        case None => tokens
        case Some(t) => createTokenList(tokens :+ t)
      }
    }
    val tokenList = createTokenList(ArrayBuffer[Token]())
    //println(tokenList)
    val parser = new Parser(tokenList.iterator.buffered)
    val ast = parser.parseProgram
    //print(ast.toString)
    val output = CodeGen.genProg(ast)
    //println(output)

    val file = new File(fileName.substring(0, fileName.length -1)+"s")
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(output)
    bw.close()
  }
}