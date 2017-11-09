package edu.towson.cosc.cosc455.mwrzos1.project1


import scala.collection.mutable.ListBuffer

class MySyntaxAnalyzer extends SyntaxAnalyzer {

  //declaration of parse tree
  var parseTree = ListBuffer[String]()

  // Starting production for gittex language
    override def gittex(): Unit = {
      //compare to current token if it match add to parse tree
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCB)) {

      //add to parse tree if it match
      parseTree.+=(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      variableDefine()
      title()
      body()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE)) {
        parseTree.+=(Compiler.currentToken)
      }
      else {
        println("Syntax Error should be: " + CONSTANTS.DOCE + " Received " + Compiler.currentToken)
        System.exit(1)
      }
    }
    else {
      println("Syntax Error should be: " + CONSTANTS.DOCB + " Received " + Compiler.currentToken)
      System.exit(1)
    }
  }

   override def title(): Unit = {

    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.TITLEB)) {
      parseTree.+=(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      // call for required text function defined at the bottom
      requiredText()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
        parseTree.+=(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else {
        println(" Syntax Error should be: " + CONSTANTS.BRACKETE + " Received " + Compiler.currentToken)
        System.exit(1)
      }
    }
    else {
      println(" Syntax Error should be : " + CONSTANTS.TITLEB + " Received " + Compiler.currentToken)
      System.exit(1)
    }
  }

  override def body(): Unit = {
    if (Compiler.length == Compiler.Scanner.position){}
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAB)||Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAE)) {
      paragraph()
      body()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEWLINE)) {
      newline()
      body()
    }
    else  {
      innerText()
      body()
    }
  }


  override def paragraph(): Unit = {

    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAB)) {
      parseTree.+=(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)) {
        variableDefine()
      }
      innerText()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAE)) {
        parseTree.+=(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else {
        println("Syntax Error should be " + CONSTANTS.PARAE + " Received " + Compiler.currentToken)
        System.exit(1)
      }
    }
    else {
      println("Syntax Error should be " + CONSTANTS.PARAB + " Received " + Compiler.currentToken)
      System.exit(1)
    }

  }
   //more than 1 RHS algorithm
   def innerText(): Unit = {

     Compiler.currentToken match {
       case CONSTANTS.USEB => variableUse(); innerText()
       case CONSTANTS.HEADING => heading(); innerText()
       case CONSTANTS.BOLD => bold(); innerText()
       case CONSTANTS.LISTITEM => listItem(); innerText()
       case CONSTANTS.IMAGEB => image(); innerText()
       case CONSTANTS.LINKB => link(); innerText()
       case _ => if (text) {
         parseTree.+=(Compiler.currentToken)
         Compiler.Scanner.getNextToken()
         innerText()
       }

     }
   }
  override def heading(): Unit = {

    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEADING)) {
      parseTree.+=(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      requiredText()
    }
  }

  override def variableDefine(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)) {
      parseTree.+=(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      requiredText()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.EQSIGN)) {
        parseTree.+=(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
        requiredText()
        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
          parseTree.+=(Compiler.currentToken)
          Compiler.Scanner.getNextToken()
          variableDefine()
        }
        else {
          println("Syntax Error should be:" + CONSTANTS.BRACKETE + " Received " + Compiler.currentToken)
          System.exit(1)
        }
      }
      else {
        println("Syntax Error should be:" + CONSTANTS.EQSIGN + " Received " + Compiler.currentToken)
        System.exit(1)
      }
    }
  }

  override def variableUse(): Unit = {

    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)) {
      parseTree.+=(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      requiredText()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
        parseTree.+=(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else {
        println("Syntax Error should be " + CONSTANTS.BRACKETE + " Received " + Compiler.currentToken)
        System.exit(1)
      }
    }
  }

 override def bold(): Unit = {

    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)) {
      parseTree.+=(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      requiredText()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)) {
        parseTree.+=(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else {
        println("Syntax Error should be" + CONSTANTS.BOLD + " Received " + Compiler.currentToken)
        System.exit(1)
      }
    }
  }


  override def listItem(): Unit = {

    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEM)) {
      parseTree.+=(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      innerItem()
      listItem()
    }
  }
   def innerItem(): Unit = {

    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)) {
      variableUse()
      innerItem()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)) {
      bold()
      innerItem()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB)) {
      link()
      innerItem()
    }
    else if (text) {
      requiredText()
      innerItem()
    }
    else {}
  }


  override def link(): Unit = {

    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB)) {
      parseTree.+=(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      requiredText()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
        parseTree.+=(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSB)) {
          parseTree.+=(Compiler.currentToken)
          Compiler.Scanner.getNextToken()
          requiredText()  // not sure about that
          if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSE)) {
            parseTree.+=(Compiler.currentToken)
            Compiler.Scanner.getNextToken()
          }
          else {
            println("Syntax Error " + CONSTANTS.ADDRESSE + " Received " + Compiler.currentToken)
            System.exit(1)
          }
        }
        else {
          println("Syntax Error " + CONSTANTS.ADDRESSB+ " Received " + Compiler.currentToken)
          System.exit(1)
        }
      }
      else {
        println("Syntax Error " + CONSTANTS.BRACKETE + " Received " + Compiler.currentToken)
        System.exit(1)
      }
    }
  }


  override def image(): Unit = {


    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB)) {
      parseTree.+=(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      requiredText()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
        parseTree.+=(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSB)) {
          parseTree.+=(Compiler.currentToken)
          Compiler.Scanner.getNextToken()
          requiredText() //not sure about that
          if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSE)) {
            parseTree.+=(Compiler.currentToken)
            Compiler.Scanner.getNextToken()
          }
          else {
            println("Syntax Error " + CONSTANTS.ADDRESSE + " Received " + Compiler.currentToken)
            System.exit(1)
          }
        }
        else {
          println("Syntax Error " + CONSTANTS.ADDRESSB + " Received " + Compiler.currentToken)
          System.exit(1)
        }
      }
      else {
        println("Syntax Error " + CONSTANTS.BRACKETE + " Received " + Compiler.currentToken)
        System.exit(1)
      }
    }
  }


    override def newline(): Unit = {

      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEWLINE)) {
        parseTree.+=(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
    }

     //function for places in the code where text is needed
     def requiredText(): Unit = {
      if (text() && !Compiler.currentToken.contains(CONSTANTS.specialChars)) {
        parseTree.+=(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
        requiredText()
      }
    }
      //Accounting for special tokens such as ':', '.' ',' in text file
     def text(): Boolean = {

       var check : Boolean = false
       if(Compiler.currentToken.filter(a=>CONSTANTS.specialChars.contains(a)).length == 0) {
         check = Compiler.currentToken.contains(',') || Compiler.currentToken.contains(':') || Compiler.currentToken.contains('.')
         if (!check) {
           if (Compiler.currentToken.last == '\n') {
             check = Compiler.currentToken.substring(0, Compiler.currentToken.length - 1).last.isLetterOrDigit
           }
           else {
             check = Compiler.currentToken.last.isLetterOrDigit
           }
         }
       }
       check
     }
}