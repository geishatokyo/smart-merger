package com.geishatokyo.smartmerger.merge

import scala.util.matching.Regex
import com.geishatokyo.smartmerger.parse.ParsedData

/**
 * Created by takeshita on 2014/06/03.
 */
sealed trait MergeBlock {
  def name : Option[String]
  def text : String
}

case class ReplaceMBlock(name : Option[String],text : String) extends MergeBlock

trait ConditionalInsertMBlock extends MergeBlock{
  def needInsert(parsedData : ParsedData) : Boolean
}

case class RegexConditionInsertMBlock(name : Option[String],regex : Regex,text : String) extends ConditionalInsertMBlock{
  override def needInsert(parsedData : ParsedData): Boolean = {
    val code = parsedData.rawString
    regex.findFirstMatchIn(code).isEmpty
  }
}
