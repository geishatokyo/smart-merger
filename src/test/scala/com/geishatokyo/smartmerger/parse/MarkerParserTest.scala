package com.geishatokyo.smartmerger.parse

import org.scalatest._

/**
 * Created by takeshita on 2014/06/03.
 */
class MarkerParserTest extends FlatSpec with Matchers {

  "MarkerParser" should "parse no marker text" in {

    val parser = MarkerParser.doubleSharpParser()

    val text = """There are no markers."""
    val blocks = parser.parse(text)

    println(blocks)

    assert(blocks.size == 1)
    assert(blocks(0) == TextBlock(text))

  }

  "MarkerParser" should "parse with marker tag" in {

    val parser = MarkerParser.doubleSharpParser()

    val text =
      """head
        |##replace[withName]
        |##end
        |hoge
        |##hold
        |here is no name
        |##end
        |aaaa
        |##insert[fuga]
        |
        |aaa
      """.stripMargin
    val blocks = parser.parse(text)

    println(blocks)

    assert(blocks.size == 7)

  }
}
