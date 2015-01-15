package com.geishatokyo.smartmerger.parse

import org.scalatest._

/**
 * Created by takeshita on 2014/06/03.
 */
class MarkerParserTest extends FlatSpec with Matchers {

  "MarkerParser" should "parse no marker text" in {

    val parser = MarkerParser.doubleSharpParser()

    val text = """There are no markers."""
    val parsedData = parser.parse(text)

    println(parsedData.blocks)

    assert(parsedData.blocks.size == 1)
    assert(parsedData.blocks(0) == TextBlock(text))

  }

  "MarkerParser" should "parse with marker tag" in {

    val parser = MarkerParser.doubleSharpParser()

    val text =
      """head
        |  ##replace[withName]
        |  ##end
        |hoge
        |##hold
        |here is no name
        |##end
        |aaaa
        |##insert[fuga]
        |
        |aaa
      """.stripMargin
    val parsedData = parser.parse(text)

    println(parsedData.blocks)

    assert(parsedData.blocks.size == 7)
    assert(parsedData.blocks(1).indent == 2)

  }

  "SkipMergeParser" should "parse skip_merge tag" in {
    val parser = MarkerParser.doubleSlashParser()

    val text =
      """head
        |//@skip_merge
        |
        |aaa
      """.stripMargin
    val parsedData = parser.parse(text)

    println(parsedData.blocks)

    assert(parsedData.blocks.size == 3)
    assert(parsedData.blocks(1) == SkipMerge("//@skip_merge"))

  }

}
