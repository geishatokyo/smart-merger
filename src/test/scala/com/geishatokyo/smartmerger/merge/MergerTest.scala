package com.geishatokyo.smartmerger.merge

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import com.geishatokyo.smartmerger.parse.{ParsedData, MarkerParser}

/**
 * Created by takeshita on 2014/06/04.
 */
class MergerTest extends FlatSpec with ShouldMatchers {

  "Replace merge" should "merge" in {
    val merger = Merger()

    val parsedData = ParsedData( MarkerParser.doubleSlashParser().parse(
      """
        |//@replace[rep1]
        |here is replaced
        |//@end
        |
        |//@replace
        |anonymous is also replaced
        |//@end
        |not replaced
        |
        |def hoge() = {}
        |
        |//@insert[ins1]
        |code is insert up on this line
        |
      """.stripMargin)
    )

    val mergeData = MergeData(
      List(
        ReplaceMBlock(Some("rep1"),"replaced!"),
        ReplaceMBlock(None,"anonymous is replaced!"),
        ReplaceMBlock(Some("rep1"),"replaced 2!"),
        ReplaceMBlock(Some("ins1"),"inserted!"),
        RegexConditionInsertMBlock(Some("ins1"),"""def\s+hoge\(\)""".r,"def hoge() = { this is not inserted.Because regex code already exists.}"),
        RegexConditionInsertMBlock(Some("ins1"),"""def\s+fuga\(\)""".r,"def fuga() = this is inserted.")
    ))

    val result = merger.merge(parsedData,mergeData)
    println(result.rawString)

    assert(result.blocks.size == parsedData.blocks.size)

  }

  "Hold merge" should "merge" in {

    val merger = Merger()

    val baseCode = ParsedData( MarkerParser.doubleSlashParser().parse(
      """
        |
        |//@hold
        |This is left.
        |//@end
        |
        |this is not left
        |
        |//@hold[hold1]
        |This is alse left.
        |//@end
        |
        |
        |
      """.stripMargin)
    )

    val mergeCode = ParsedData(MarkerParser.doubleSlashParser().parse(
      """
        |
        |Yes
        |//@hold
        |This is not merged
        |//@end
        |
        |Oh, yes!
        |
        |//@hold[hold1]
        |This is not alse merged.
        |//@end
        |Yes!Yes!
      """.stripMargin
    ))

    val result = merger.merge(baseCode,mergeCode)

    println(result.rawString)
    assert(result.blocks.size == baseCode.blocks.size)

  }

}