package edu.towson.cosc.cosc455.mwrzos1.project1

class MyLexicalAnalyzer extends LexicalAnalyzer {

  //values declaration

  var nchar: Char = ' '
  var tokens: String = ""
  var position: Int = 0

  override def addChar(): Unit = {

    // add characters to token
    tokens += nchar


  }

  override def getChar(): Unit = {


    nchar = Compiler.fileContents.charAt(position)

    // increase position by 1 each time you read character
    position += 1

  }


  // reset
  def resetToken(): Unit = {

    tokens = ""

  }

  override def getNextToken(): Unit = {
    tokens = ""
    getChar()
    nonSpace()
    if (CONSTANTS.specialChars.contains(nchar)) {

      if (CONSTANTS.BOLD.contains(nchar)) {
        addChar()
        getChar()
      }
      else if (CONSTANTS.LISTITEM.contains(nchar)) {
        addChar()
        tokens += read()
      }
    }
    if (CONSTANTS.specialChars(4) == (nchar)) {
      addChar()
      tokens += read()
    }
    if (CONSTANTS.BRACKETE.contains(nchar)) {
      addChar()

      if (CONSTANTS.DOCE.contains(tokens.toUpperCase())) {
        nonSpace()
        if (position != Compiler.length) {
          position = position - 1
          getNextToken()
          println("Error can't be anything after document end")
          System.exit(1)
        }
      }
    }

    else if (CONSTANTS.HEADING.contains(nchar)) {
      addChar()
      tokens += read()
    }
    else if (CONSTANTS.IMAGEB.charAt(0) == nchar) {
      addChar()
      getChar()
      if (CONSTANTS.BRACKETE.contains(nchar)) {
        addChar()
      }
      else {
        println("Lexical error. Wrong character. Received:" + nchar)
        System.exit(1)
      }
    }

    tokens = tokens.map(_.toUpper)

    if (tokens.endsWith("\n")) {
      tokens = tokens.substring(0, tokens.length - 1)
    }
    if (lookup()) {
      Compiler.currentToken = tokens
    }
    else if (nchar.isLetterOrDigit || nchar.equals(':') || nchar.equals('.') || nchar.equals(',')) {
      addChar()
      tokens += read()
      if ((nchar.equals(CONSTANTS.BRACKETE) || nchar.equals(CONSTANTS.PARAE) || nchar.equals(CONSTANTS.EQSIGN) || nchar.equals('\\'))) {
        //Will decrement index so special characters aren't skipped
        position -= 1
      }
      Compiler.currentToken = tokens
    }
    else if (nchar.equals('\n') || nchar.equals('\r') || nchar.equals('\t')) {
      getNextToken() //Skip and get next token
    }
    else {
      println("Lexical error: Illegal character received: '" + nchar + "'")
      System.exit(1)
    }
  }

    def lookup(): Boolean = {

      if (CONSTANTS.Constants.contains(tokens.toUpperCase())) return true
      else return false
    }


    def read(): String = {
      var text: String = ""
      getChar()

      while (position < Compiler.length && !CONSTANTS.EOFL.contains(nchar) && !CONSTANTS.specialChars.contains(nchar)) {
        text += nchar

        getChar()
      }
      nchar match {

        case ('\n') => text += nchar
        case ('\t') => text += nchar
        case ('\r') => text += nchar
        case _ => text += nchar
      }
      return text
    }

    def nonSpace(): Unit = {

      while ((nchar.equals(' ') || nchar.equals('\n') || nchar.equals('\r') || nchar.equals('\t'))) {
        getChar()
      }
    }

}