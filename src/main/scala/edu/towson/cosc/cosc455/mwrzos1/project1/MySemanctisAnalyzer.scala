package edu.towson.cosc.cosc455.mwrzos1.project1

import java.io._
import java.awt.Desktop
import java.io.{File, IOException}

class MySemanctisAnalyzer {

  // variables declaration
  var Stack = new scala.collection.mutable.Stack[String]
  var Count1 = 0
  var varName = new scala.collection.mutable.Queue[String]
  var varVal = new scala.collection.mutable.Queue[String]
  var scope: Int = 0

  //implementation of function that converts stack to HTML
  def toHTML() = {
    val file = new PrintWriter(new File("outputFile.html"))
    //  conversion the list buffer to stack structure
    for (a <- Compiler.Parser.parseTree) {
      Stack.push(a)
    }
    Stack = Stack.reverse
    while (!Stack.isEmpty) {
      var currentToken: String = Stack.pop()
      currentToken.toUpperCase() match {
        case CONSTANTS.DOCB => file.append("<html>\n")
        case CONSTANTS.DOCE => file.append("\n</html>")
        case CONSTANTS.TITLEB => file.append("<head>\n<title> ");
          currentToken = Stack.pop()

          while (!currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
            file.append(currentToken + " ")
            currentToken = Stack.pop()
          }
          file.append(" </title>\n</head>\n")

        case CONSTANTS.HEADING => file.append("<h1> ");
          currentToken = Stack.pop()
          while (!CONSTANTS.Constants.contains(currentToken)) {
            file.append(currentToken + " ")
            currentToken = Stack.pop()
          }
          Stack.push(currentToken)
          file.append(" </h1>\n")

        case CONSTANTS.PARAB => file.append("<p> "); scope = 1
        case CONSTANTS.PARAE => file.append(" </p>\n")
          if (scope == 1 && Count1 > 0) {
            for (a <- 0 until Count1) varName.dequeue()
          }
        case CONSTANTS.LINKB => var linkText: String = ""; var url: String = ""; currentToken = Stack.pop()
          while (!currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
            linkText = linkText + currentToken + " "
            currentToken = Stack.pop()
          }
          Stack.pop()
          url += Stack.pop()
          Stack.pop()

          file.append("<a href=\"" + url + "\">" + linkText + "</a> ")

        case CONSTANTS.LISTITEM => file.append("\n<li> ");
          currentToken = Stack.pop()
          if (currentToken.contains("\n")) {
            file.append(currentToken + " </li>")
          }
          else if (currentToken.equalsIgnoreCase(CONSTANTS.USEB)) {
            Stack.push(currentToken)
          }
          else {
            if (!currentToken.equalsIgnoreCase(CONSTANTS.USEB)) {
              file.append(currentToken + " ")
              currentToken = Stack.top
              if (currentToken.contains("\n")) {
                currentToken = Stack.pop()
                file.append(currentToken + " </li>")
              }
            }
          }


        case CONSTANTS.NEWLINE => file.append("<br>\n")

        case CONSTANTS.IMAGEB => var linkText: String = "" ; var url: String = "";
          currentToken = Stack.pop()
          while (!currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
            linkText = linkText + currentToken + " "
            currentToken = Stack.pop()
          }
          Stack.pop()
          url += Stack.pop()
          Stack.pop()

          file.append("<img src=\"" + url + "\" alt=" + linkText + "\">")

        case CONSTANTS.BOLD => var innerText: String = "";
          currentToken = Stack.pop()
          while (!currentToken.equalsIgnoreCase(CONSTANTS.BOLD)) {
            innerText += currentToken
            currentToken = Stack.pop()
          }
          file.append("<b> " + innerText + " </b>")

        case CONSTANTS.DEFB =>
          varName.enqueue(Stack.pop())
          Stack.pop()
          varVal.enqueue(Stack.pop())
          Stack.pop()
          if (scope == 1) Count1 += 1

        case CONSTANTS.USEB => var useVar: String = Stack.pop()
          if (varName.contains(useVar)) {
            file.append(varVal(varName.indexOf(useVar, scope)) + " ")
          }
          else {
            println("Static semantic error. Variable '" + useVar + "'  not defined")
            System.exit(1)
          }
          Stack.pop()
        case _ => file.append(currentToken + " ")
      }
    }
    file.close()
    openHTMLFileInBrowser("outputFile.html")
  }
   // function that takes a String file name and opens in web browser
  def openHTMLFileInBrowser(htmlFileStr: String) = {
    val file: File = new File(htmlFileStr.trim)
    println(file.getAbsolutePath)
    if (!file.exists())
      sys.error("File " + htmlFileStr + "  not exist.")

    try {
      Desktop.getDesktop.browse(file.toURI)
    }
    catch {
      case ioe: IOException => sys.error("Failed to open file:  " + htmlFileStr)
    }
  }
}
