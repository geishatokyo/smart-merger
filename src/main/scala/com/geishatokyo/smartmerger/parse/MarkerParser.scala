package com.geishatokyo.smartmerger.parse

import com.geishatokyo.smartmerger.util.TrieMap
import scala.util.matching.Regex

/**
 * Created by takeshita on 2014/05/30.
 */
object MarkerParser{

  def apply() : MarkerParser = doubleSlashParser()

  /**
   * コメントアウトが//のタイプの言語用パーサー
   * @return
   */
  def doubleSlashParser() = {
    new MarkerParser(Replace("//@replace", "//@end") :: Hold("//@hold", "//@end") :: Insert("//@insert") :: SkipMergeParser("//@skip_merge") :: Nil)
  }

  /**
   * コメントアウトが#タイプの言語用パーサー
   * @return
   */
  def doubleSharpParser() = {
    new MarkerParser(Replace("##replace", "##end") :: Hold("##hold", "##end") :: Insert("##insert") :: SkipMergeParser("##skip_merge") :: Nil)
  }

  def xmlParser() = {
    new MarkerParser(Replace("<!--@replace-->", "<!--@end-->") :: Hold("<!--@hold-->", "<!--@end-->") :: Insert("<!--@insert-->") :: SkipMergeParser("<!--@skip_merge-->") :: Nil)
  }
}

/**
 * コード中の置換用マーカーのパースを行う。
 * @param blockParsers
 */
class MarkerParser(blockParsers : List[BlockParser[Block]]) {

  //var blockParsers: List[BlockParser[Block]] = Replace("##replace", "##end") :: Hold("##hold", "##end") :: Insert("##insert") :: Nil


  def parse(file: String) : ParsedData = {

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
            case Some( (block,parsedCharLength)) => {
              if (buffer.size > 0) {
                blocks = TextBlock(buffer.toString) :: blocks
                buffer.clear()
              }
              blocks = block :: blocks
              index += parsedCharLength
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

    ParsedData(blocks.reverse :_*)

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
  override def toBlock(name: String, text: String, indent : Int): ReplaceBlock = {
    ReplaceBlock(startTag, name,endTag,text,indent)
  }
}

case class Hold(val startTag : String,val endTag : String) extends PairTagBlockParser[HoldBlock]{
  override def toBlock(name: String, text: String, indent : Int): HoldBlock = {
    HoldBlock(startTag, name,endTag,text,indent)
  }
}
case class Insert(val startTag : String) extends SingleTagBlockParser[InsertPoint]{
  override def toBlock(name: String, text: String, indent : Int) : InsertPoint  = {
    InsertPoint(startTag,name,text,indent)
  }
}


trait BlockParser[+T <: Block] {

  /**
   * 開始タグ
   * @return
   */
  def startTag: String

  /**
   * 開始タグが見つかった場合に呼ばれる。
   * ブロックをぱーすして、その結果を返す
   * @param s ファイルの内容全体
   * @param fromIndex 開始タグの次の文字の位置
   * @param context
   * @return parsedBlock and parse character length
   */
  def parse(s : String,fromIndex : Int)(implicit context : Context) : Option[(T,Int)]


  lazy val nameRegex = (Regex.quote(startTag) + """\[([\S^\]]+)\]""").r
  protected def getName(line : String) : Option[String] = {
    nameRegex.findPrefixMatchOf(line) match{
      case Some(m) => {
        Some(m.group(1))
      }
      case None => None
    }
  }

  def getIndent(s : String,fromIndex : Int) : Int = {
    for(i <- (fromIndex - 1) to 0 by -1){
      val c = s.charAt(i)
      if(c == '\n' || c == '\r'){
        return fromIndex - i - 1
      }
    }
    fromIndex
  }

}
trait PairTagBlockParser[+T <: Block] extends BlockParser[T]{
  def endTag : String

  private val nameRule = AnonymousBlockNameRule

  def parse(s : String,fromIndex : Int)(implicit context : Context) = {

    val i = s.indexOf(endTag,fromIndex)
    if(i < 0) None
    else{

      val indent = getIndent(s,fromIndex)

      val _name = getName(s.substring(fromIndex,(fromIndex + 50).min(s.length)))
      val name = _name.getOrElse(nameRule.getName( context.nextBlockId))
      val offset = startTag.length + _name.map(_.length + 2).getOrElse(0)
      Some( (toBlock(name,s.substring(fromIndex + offset,i),indent),i - fromIndex + endTag.length) )
    }
  }

  def toBlock(name : String,text : String, indent : Int) : T
}

trait SingleTagBlockParser[+T <: Block] extends BlockParser[T]{

  private val nameRule = AnonymousBlockNameRule
  def parse(s : String,fromIndex : Int)(implicit context : Context) = {
    val indent = getIndent(s,fromIndex)
    getName(s.substring(fromIndex)) match{
      case Some(name) => Some(toBlock(name,"",indent) -> (startTag.length + name.length + 2))
      case None => Some(toBlock(nameRule.getName(context.nextBlockId),"",indent) -> startTag.length )
    }
  }

  def toBlock(name : String,text : String,indent : Int) : T
}

case class SkipMergeParser(startTag : String) extends BlockParser[SkipMerge ]{
  override def parse(s: String, fromIndex: Int)(implicit context: Context) = {
    Some(SkipMerge(startTag) -> startTag.length)
  }
}
