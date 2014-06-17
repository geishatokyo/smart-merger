package com.geishatokyo.smartmerger.injection

import scala.util.matching.Regex
import com.geishatokyo.smartmerger.parse.ParsedData

/**
 * Created by takeshita on 2014/06/03.
 */
sealed trait InjectionBlock {
  def name : Option[String]
  def text : String
}

case class ReplaceInjection(name : Option[String],text : String) extends InjectionBlock

trait ConditionalInjection extends InjectionBlock{
  def needInsert(parsedData : ParsedData) : Boolean
}

case class RegexConditionInjection(name : Option[String],regex : Regex,text : String) extends ConditionalInjection{
  override def needInsert(parsedData : ParsedData): Boolean = {
    val code = parsedData.rawString
    regex.findFirstMatchIn(code).isEmpty
  }
}
case class ContainsConditionInjection(name : Option[String], matchText : String,text : String) extends ConditionalInjection{
  override def needInsert(parsedData : ParsedData): Boolean = {
    val code = parsedData.rawString
    !code.contains(matchText)
  }
}
