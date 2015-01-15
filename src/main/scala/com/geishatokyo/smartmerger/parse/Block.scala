package com.geishatokyo.smartmerger.parse

/**
 * Created by takeshita on 2014/06/03.
 */

trait Block{
  def text : String

  def fullText : String

  def copyWithText(text : String) : Block
  def indent : Int

}
trait BlockWithStartTag extends Block{
  def startTag : String
  def name : String

}
trait BlockWithEndTag extends BlockWithStartTag{
  def endTag : String
}

case class TextBlock(text : String) extends Block{
  override def copyWithText(text: String): Block = this.copy(text = text)
  def indent = 0

  override def fullText: String = text
}
case class InsertPoint(startTag : String,name : String,text : String,indent : Int) extends BlockWithStartTag{


  def copyWithText(t : String) = {
    copy(text = t)
  }

  override def fullText: String = {
    text + System.lineSeparator() + (" " * indent)
  }
}
case class ReplaceBlock(startTag : String,name : String,endTag : String,text : String,indent : Int) extends BlockWithEndTag{

  def copyWithText(t : String) = {
    copy(text = t)
  }

  override def fullText: String = {
    startTag + text + System.lineSeparator() + (" " * indent)
  }
}
case class HoldBlock(startTag : String,name : String, endTag : String, text : String,indent : Int) extends BlockWithEndTag{

  def copyWithText(t : String) = {
    copy(text = t)
  }
  override def fullText: String = {
    startTag + text + System.lineSeparator() + (" " * indent)
  }
}

case class SkipMerge(startTag : String) extends Block{
  override def text: String = startTag

  def copyWithText(t : String) = {
    this
  }
  def indent = 0

  override def fullText: String = {
    startTag
  }
}