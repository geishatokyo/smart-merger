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
    new MarkerParser(
      CommentBlock("//",None) :: Nil,
      Replace("@replace", "@end") :: Hold("@hold", "@end") :: Insert("@insert") :: SkipMergeParser("@skip_merge") :: Nil)
  }

  /**
   * コメントアウトが#タイプの言語用パーサー
   * @return
   */
  def doubleSharpParser() = {
    new MarkerParser(
      CommentBlock("##",None) :: Nil,
      Replace("replace", "end") :: Hold("hold", "end") :: Insert("insert") :: SkipMergeParser("skip_merge") :: Nil)
  }

  def xmlParser() = {
    new MarkerParser(
      CommentBlock("<!--",Some("-->")) :: Nil,
      Replace("@replace", "@end") :: Hold("@hold", "@end") :: Insert("@insert") :: SkipMergeParser("@skip_merge") :: Nil)
  }


  /**
   * コメントアウト--タイプの言語用パーサー
   * @return
   */
  def doubleHyphenParser() = {
    new MarkerParser(
      CommentBlock("--",None) :: Nil,
      Replace("@replace", "@end") :: Hold("@hold", "@end") :: Insert("@insert") :: SkipMergeParser("@skip_merge") :: Nil)
  }
}

case class CommentBlock(start : String,end : Option[String]){

  def envelop(s : String) = start + s + end.getOrElse("")
}
/**
 * コード中の置換用マーカーのパースを行う。
 * @param blockParsers
 */
class MarkerParser(comments : List[CommentBlock],blockParsers : List[BlockParser[Block]]) {

  //var blockParsers: List[BlockParser[Block]] = Replace("##replace", "##end") :: Hold("##hold", "##end") :: Insert("##insert") :: Nil


  def parse(file: String) : ParsedData = {

    val matcher = TrieMap[BlockParser[Block]](blockParsers.map(p => p.startTag -> p): _*)

    val maxStartTagLength = matcher.depth
    implicit val context = new Context

    val buffer = new StringBuilder(file.length)

    var blocks: List[Block] = Nil
    val reader = new StringReader(file)

    while(!reader.isEnd){
      val old = reader.index


      comments.find(c => reader.startsWith(c.start)) match{
        case Some(c) => {
          blockParsers.view.map(parser => parser.parse(reader,c)).find(_.isDefined).flatten match{
            case Some(block) => {
              if(buffer.length > 0){
                blocks = TextBlock(buffer.toString()) :: blocks
                buffer.clear()
              }
              blocks = block :: blocks
            }
            case None => {
              reader.index = old
              buffer.append(reader.current)
              reader.next(1)
            }
          }
        }
        case None => {
          buffer.append(reader.current)
          reader.next(1)
        }
      }
    }

    if (buffer.size > 0) {
      blocks = TextBlock(buffer.toString) :: blocks
    }

    ParsedData(blocks.reverse :_*)

  }


}
class StringReader(val s : String){
  var index = 0
  def take(n : Int) = {
    s.substring(index,(index + n).min(s.length))
  }
  def next(step : Int) = index += step

  def startsWith(s : String) = {
    if(length < s.length) false
    else take(s.length) == s
  }

  def isEnd = index >= s.length
  def length = s.length - index

  val whitespaces = " \n\r\t"

  def skipWhiteSpaces() = {
    while(index < s.length && whitespaces.contains(s.charAt(index))){
      index += 1
    }
  }
  def current = s.charAt(index)

  def indexOf(v : String) = s.indexOf(v,index)

  def readUntil(end : String) = {
    val i = indexOf(end)
    if(i > 0){
      val v = s.substring(index,i)
      index = i + end.length
      Some(v)
    }else None
  }

}
class Context {
  var anonymousBlockCount = 0

  def nextBlockId = {
    anonymousBlockCount += 1
    anonymousBlockCount
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
   * @param reader
   * @param context
   * @return parsedBlock and parse character length
   */
  def parse(reader : StringReader,commentBlock : CommentBlock)(implicit context : Context) : Option[T]


  lazy val nameRegex = (Regex.quote(startTag) + """\[([\S^\]]+)\]""").r
  protected def getName(reader : StringReader) : Option[String] = {
    val old = reader.index
    reader.skipWhiteSpaces()
    if(reader.current == '['){
      reader.next(1)
      val name = reader.readUntil("]")
      name.map(_.trim)
    }else {
      reader.index = old
      None
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

  def parse(reader : StringReader,commentBlock : CommentBlock)(implicit context : Context) : Option[T] = {
    val startIndex = reader.index

    val trueStartTag = commentBlock.start + startTag
    val trueEndTag = commentBlock.envelop(endTag)
    if(!reader.startsWith(trueStartTag)) return None
    reader.next(trueStartTag.length)
    val i = reader.indexOf(trueEndTag)
    if(i < 0) {
      None
    }else{
      val indent = getIndent(reader.s,startIndex)

      val _name = getName(reader)
      val name = _name.getOrElse(nameRule.getName( context.nextBlockId))
      val body = if(commentBlock.end.isDefined){
        reader.next(commentBlock.end.get.length)
        reader.readUntil(trueEndTag)
      }else{
        reader.readUntil(trueEndTag)
      }

      Some( toBlock(name,body.getOrElse(""),indent,commentBlock) )
    }
  }

  def toBlock(name : String,text : String, indent : Int,commentBlock: CommentBlock) : T
}

trait SingleTagBlockParser[+T <: Block] extends BlockParser[T]{

  private val nameRule = AnonymousBlockNameRule
  def parse(reader : StringReader,commentBlock : CommentBlock)(implicit context : Context) : Option[T] = {
    val startIndex = reader.index
    val trueStartTag = commentBlock.start + startTag
    if(!reader.startsWith(trueStartTag)) return None
    reader.next(trueStartTag.length)
    val indent = getIndent(reader.s,startIndex)
    getName(reader) match{
      case Some(name) => Some(toBlock(name,"",indent,commentBlock))
      case None => Some(toBlock(nameRule.getName(context.nextBlockId),"",indent,commentBlock))
    }
  }

  def toBlock(name : String,text : String,indent : Int,commentBlock : CommentBlock) : T
}

case class Replace(val startTag : String,val endTag : String) extends PairTagBlockParser[ReplaceBlock]{
  override def toBlock(name: String, text: String, indent : Int,commentBlock: CommentBlock): ReplaceBlock = {
    ReplaceBlock(startTag, name,endTag,text,indent,commentBlock)
  }
}

case class Hold(val startTag : String,val endTag : String) extends PairTagBlockParser[HoldBlock]{
  override def toBlock(name: String, text: String, indent : Int,commentBlock: CommentBlock): HoldBlock = {
    HoldBlock(startTag, name,endTag,text,indent,commentBlock)
  }
}
case class Insert(val startTag : String) extends SingleTagBlockParser[InsertPoint]{
  override def toBlock(name: String, text: String, indent : Int,commentBlock: CommentBlock) : InsertPoint  = {
    InsertPoint(startTag,name,text,indent,commentBlock)
  }
}
case class SkipMergeParser(startTag : String) extends BlockParser[SkipMerge ]{
  override def parse(reader : StringReader,commentBlock : CommentBlock)(implicit context: Context) : Option[SkipMerge] = {

    val trueStartTag = commentBlock.start + startTag
    if(!reader.startsWith(trueStartTag)) return None
    reader.next(trueStartTag.length)
    Some(SkipMerge(startTag,commentBlock))
  }
}
