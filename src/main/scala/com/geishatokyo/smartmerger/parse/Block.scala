package com.geishatokyo.smartmerger.parse

/**
 * Created by takeshita on 2014/06/03.
 */

trait Block{
  def text : String
}
trait BlockWithStartTag extends Block{
  def startTag : String
  def name : String

}
trait BlockWithEndTag extends BlockWithStartTag{
  def endTag : String
}

case class TextBlock(text : String) extends Block
case class InsertPoint(startTag : String,name : String,text : String) extends BlockWithStartTag{

  val nameRule = AnonymousBlockNameRule

  def copyWithTextWithoutTag(t : String) = {

    val st = if(name.startsWith(nameRule.prefix)) startTag else s"${startTag}[$name]"

    copy(text = st + System.lineSeparator() + t)
  }
}
case class ReplaceBlock(startTag : String,name : String,endTag : String,text : String) extends BlockWithEndTag{

  val nameRule = AnonymousBlockNameRule
  def copyWithTextWithoutTag(t : String) = {
    val st = if(name.startsWith(nameRule.prefix)) startTag else s"${startTag}[$name]"
    copy(text = st + System.lineSeparator() + t + System.lineSeparator() + endTag)
  }
}
case class HoldBlock(startTag : String,name : String, endTag : String, text : String) extends BlockWithEndTag{

  val nameRule = AnonymousBlockNameRule
  val st = if(name.startsWith(nameRule.prefix)) startTag else s"${startTag}[$name]"
  def copyWithTextWithoutTag(t : String) = {
    copy(text = st + System.lineSeparator() + t + System.lineSeparator() + endTag)
  }
}

case class SkipMerge(startTag : String) extends Block{
  override def text: String = startTag
}