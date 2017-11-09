package edu.towson.cosc.cosc455.mwrzos1.project1

object Compiler {


  var currentToken : String = ""
  var fileContents : String = ""
  var filename : String= ""
  val Scanner = new MyLexicalAnalyzer
  val Parser = new MySyntaxAnalyzer
  val Semantic = new MySemanctisAnalyzer
  var length: Int = 0

  def main(args: Array[String]): Unit = {

    //check of arguments and file extension
    checkFile(args)
    filename = args(0)
    readFile(args(0))

     length =fileContents.length-1

    //getting the first token
    Scanner.getNextToken()
    // start parsing the file
    Parser.gittex()
    // semantic class convert to HTML
    Semantic.toHTML()
  }

  def readFile(file : String) = {
    val source = scala.io.Source.fromFile(file)
    fileContents = try source.mkString finally source.close()
  }

  def checkFile(args : Array[String]) = {
    if (args.length != 1) {
      println("USAGE ERROR: wrong number of args fool!")
      System.exit(1)
    }
    else if (! args(0).endsWith(".gtx")) {
      println("USAGE ERROR: wrong extension fool!")
      System.exit(1)
    }
  }
}
