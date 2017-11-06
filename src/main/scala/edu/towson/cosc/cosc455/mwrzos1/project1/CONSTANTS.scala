package edu.towson.cosc.cosc455.mwrzos1.project1

import scala.collection.mutable.ListBuffer
object CONSTANTS {


  //declaration of constants for Gittex

  val DOCB : String = 	"\\BEGIN"
  val DOCE : String = 	"\\END"
  val TITLEB : String = "\\TITLE["
  val BRACKETE : String = "]"
  val HEADING : String = "#"
  val PARAB : String = "\\PARAB"
  val PARAE : String = "\\PARAE"
  val BOLD : String = "*"
  val LISTITEM : String = "+"
  val NEWLINE : String = "\\\\"
  val LINKB : String = "["
  val ADDRESSB : String = "("
  val ADDRESSE : String = ")"
  val IMAGEB : String = "!["
  val DEFB : String = "\\DEF["
  val EQSIGN : String = "="
  val USEB : String = "\\USE["


    // declaration of list consists of constants

  val Constants : List[String] = List(DOCB, DOCE, TITLEB, BRACKETE, HEADING, PARAB, PARAE, BOLD, LISTITEM,
    NEWLINE, LINKB, ADDRESSB, ADDRESSE, IMAGEB, DEFB, EQSIGN, USEB)


   //declaration of special characters for gittex

  var specialChars: ListBuffer[Char] = new ListBuffer[Char]
  val asterisk: Char  =  '*'
  val hash: Char  =  '#'
  val plus: Char = '+'
  val equal: Char  =  '='
  val backslash: Char  =  '\\'
  val exclamation: Char = '!'
  val leftBracket: Char = '['
  val rightBracket: Char = ']'
  val leftParentheses: Char = '('
  val rightParentheses: Char = ')'

  specialChars += asterisk
  specialChars += hash
  specialChars += plus
  specialChars += equal
  specialChars += backslash
  specialChars += exclamation
  specialChars += leftBracket
  specialChars += rightBracket
  specialChars += leftParentheses
  specialChars += rightParentheses


 // declaring end of line array
  val EOFL: Array[Char] = Array ('\r', '\t', ' ', '\n')


}
