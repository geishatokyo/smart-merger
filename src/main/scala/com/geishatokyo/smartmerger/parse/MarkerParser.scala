package com.geishatokyo.smartmerger.parse

import com.geishatokyo.smartmerger.util.TrieMap
import scala.util.matching.Regex

/**
 * Created by takeshita on 2014/05/30.
 */
object MarkerParser{

  def apply() : MarkerParser = doubleSlashParser()

  def doubleSlashParser() = {
    new MarkerParser(Replace("//@replace", "//@end") :: Hold("//@hold", "//@end") :: Insert("//@insert") :: Nil)
  }
  def doubleSharpParser() = {
    new MarkerParser(Replace("##replace", "##end") :: Hold("##hold", "##end") :: Insert("##insert") :: Nil)
  }

}

class MarkerParser(blockParsers : List[BlockParser[Block]]) {

  //var blockParsers: List[BlockParser[Block]] = Replace("##replace", "##end") :: Hold("##hold", "##end") :: Insert("##insert") :: Nil


  def parse(file: String) = {

    val matcher = TrieMap[BlockParser[Block]](blockParsers.map(p => p.startTag -> p): _*)

    val maxStartTagLength = matcher.depth
    implicit val context = new Context

    val buffer = new StringBuilder(file.length)

    var blocks: List[Block] = Nil
    var index = 0
    val end = file.length - maxStartTagLength
    while (index < end) {
      matcher.getLongest(file.substring(index, index + maxStartTagLength)) match {
        case Some(parser) => {
          parser.parse(file, index) match {
            case Some(block) => {
              if (buffer.size > 0) {
                blocks = TextBlock(buffer.toString) :: blocks
                buffer.clear()
              }
              blocks = block :: blocks
              index += block.text.length
            }
            case None => {
              buffer.append(file.charAt(index))
              index += 1
            }
          }
        }
        case None => {
          buffer.append(file.charAt(index))
          index += 1
        }
      }

    }
    if (index < file.size) {
      buffer.append(file.substring(index))
    }
    if (buffer.size > 0) {
      blocks = TextBlock(buffer.toString) :: blocks
    }

    blocks.reverse

  }
}
class Context {
  var anonymousBlockCount = 0

  def nextBlockId = {
    anonymousBlockCount += 1
    anonymousBlockCount
  }
}


case class Replace(val startTag : String,val endTag : String) extends PairTagBlockParser[ReplaceBlock]{
  override def toBlock(name: String, text: String): ReplaceBlock = {
    ReplaceBlock(startTag, name,endTag,text)
  }
}

case class Hold(val startTag : String,val endTag : String) extends PairTagBlockParser[HoldBlock]{
  override def toBlock(name: String, text: String): HoldBlock = {
    HoldBlock(startTag, name,endTag,text)
  }
}
case class Insert(val startTag : String) extends SingleTagBlockParser[InsertPoint]{
  override def toBlock(name: String, text: String)  = {
    InsertPoint(startTag,name,text)
  }
}


trait BlockParser[+T <: Block] {

  def startTag: String
  def parse(s : String,fromIndex : Int)(implicit context : Context) : Option[T]


  lazy val nameRegex = (Regex.quote(startTag) + """\[([\S^\]]+)\]""").r
  protected def getName(line : String) : Option[String] = {
    nameRegex.findPrefixMatchOf(line) match{
      case Some(m) => {
        Some(m.group(1))
      }
      case None => None
    }
  }

}
trait PairTagBlockParser[+T <: Block] extends BlockParser[T]{
  def endTag : String

  private val nameRule = AnonymousBlockNameRule

  def parse(s : String,fromIndex : Int)(implicit context : Context) = {

    val i = s.indexOf(endTag,fromIndex)
    if(i < 0) None
    else{
      val name = getName(s.substring(fromIndex,(fromIndex + 50).min(s.length))).getOrElse(nameRule.getName( context.nextBlockId))
      Some(toBlock(name,s.substring(fromIndex,i + endTag.length)))
    }
  }

  def toBlock(name : String,text : String) : T
}

trait SingleTagBlockParser[+T <: Block] extends BlockParser[T]{

  private val nameRule = AnonymousBlockNameRule
  def parse(s : String,fromIndex : Int)(implicit context : Context) = {
    getName(s.substring(fromIndex)) match{
      case Some(name) => Some(toBlock(name,s.substring(fromIndex,fromIndex + startTag.length + name.length + 2)))
      case None => Some(toBlock(nameRule.getName(context.nextBlockId),s.substring(fromIndex,fromIndex + startTag.length)))
    }
  }

  def toBlock(name : String,text : String) : T
}


