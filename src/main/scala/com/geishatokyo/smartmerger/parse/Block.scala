package com.geishatokyo.smartmerger.parse

import com.geishatokyo.smartmerger.Env

/**
 * Created by takeshita on 2014/06/03.
 */

trait Block{
  def text : String

  def fullText : String

  def copyWithText(text : String) : Block
  def indent : Int


  protected def addLineSeparator(t : String) : String = {
    if(t.length == 0) return ""
    val end = if(t.endsWith("\n") || t.endsWith("\r") ){
      ""
    }else Env.lineSeparator + " " * indent

    val firstLine = t.lines.next()
    if(firstLine.trim.length != 0){
      Env.lineSeparator + t + end
    }else{
      t + end
    }
  }
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
case class InsertPoint(startTag : String,name : String,text : String,indent : Int,commentBlock : CommentBlock) extends BlockWithStartTag{


  def copyWithText(t : String) = {
    copy(text = t)
  }

  override def fullText: String = {
    commentBlock.envelop(s"${startTag}[${name}]") + addLineSeparator(text)
  }
}
case class ReplaceBlock(startTag : String,name : String,endTag : String,text : String,indent : Int,commentBlock : CommentBlock) extends BlockWithEndTag{

  def copyWithText(t : String) = {
    copy(text = t)
  }

  override def fullText: String = {
    commentBlock.envelop(s"${startTag}[${name}]") + addLineSeparator(text) + commentBlock.envelop(endTag)
  }
}
case class HoldBlock(startTag : String,name : String, endTag : String, text : String,indent : Int,commentBlock : CommentBlock) extends BlockWithEndTag{

  def copyWithText(t : String) = {
    copy(text = t)
  }
  override def fullText: String = {
    commentBlock.envelop(s"${startTag}[${name}]") + addLineSeparator(text) + commentBlock.envelop(endTag)
  }
}

case class SkipMerge(startTag : String,commentBlock : CommentBlock) extends Block{
  override def text: String = startTag

  def copyWithText(t : String) = {
    this
  }
  def indent = 0

  override def fullText: String = {
    commentBlock.envelop( startTag)
  }
}