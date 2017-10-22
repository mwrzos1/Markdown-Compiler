package edu.towson.cis.cosc455.mwrzos1.project1

import scala.collection.mutable.ListBuffer

class MySyntaxAnalyzer extends SyntaxAnalyzer {

  //declaration of parse tree
 var parseTree = ListBuffer[String]()

  // Starting production for gittex
  override def gittex(): Unit = {

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
  def title() : Unit = {

    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.TITLEB)) {
    parseTree.+=(Compiler.currentToken)
    Compiler.Scanner.getNextToken()
    requiredText()
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
    parseTree.+=(Compiler.currentToken)
    }
    else {
    println(" Syntax Error should be: " + CONSTANTS.BRACKETE + " Received " + Compiler.currentToken)
    System.exit(1)
    }
   }
    else {
    println(" Syntax Error be should be : " + CONSTANTS.TITLEB + " Received " + Compiler.currentToken)
    System.exit(1)
    }
  }

  def body() : Unit
  def paragraph() : Unit
  def heading() : Unit
  def variableDefine() : Unit
  def variableUse() : Unit
  def bold() : Unit
  def listItem() : Unit
  def link() : Unit
  def image() : Unit
  def newline() : Unit

  def requiredText(): Unit = {
    if (Compiler.currentToken.equals(CONSTANTS.specialChars)) {}
    else  {
      parseTree.+=(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      requiredText()
    }

  }

  }