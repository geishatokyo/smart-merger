package com.geishatokyo.smartmerger.injection

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import com.geishatokyo.smartmerger.parse.{ParsedData, MarkerParser}

/**
 * Created by takeshita on 2014/06/04.
 */
class InjectorTest extends FlatSpec with ShouldMatchers {

  "Replace merge" should "merge" in {
    val merger = Injector()

    val parsedData =  MarkerParser.doubleSlashParser().parse(
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
        |//@insert[ins2]
        |a
        |a
        |
      """.stripMargin)

    val mergeData = InjectionData(
        ReplaceInjection(Some("rep1"),"replaced!"),
        ReplaceInjection(None,"anonymous is replaced!"),
        ReplaceInjection(Some("rep1"),"replaced 2!"),
        ReplaceInjection(Some("ins1"),"inserted!"),
        RegexConditionInjection(Some("ins1"),"""def\s+hoge\(\)""".r,"def hoge() = { this is not inserted.Because regex code already exists.}"),
        RegexConditionInjection(Some("ins1"),"""def\s+fuga\(\)""".r,"def fuga() = this is inserted."),
        RegexConditionInjection(Some("ins2"),"""def\s+hoge\(\)""".r,"def hoge2() = this is inserted.")
    )

    val result = merger.merge(parsedData,mergeData)
    println(result.rawString)

    assert(result.blocks.size == parsedData.blocks.size)

  }

  "Hold merge" should "merge" in {

    val merger = Injector()

    val baseCode = MarkerParser.doubleSlashParser().parse(
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

    val mergeCode = MarkerParser.doubleSlashParser().parse(
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
    )

    val result = merger.merge(baseCode,mergeCode)

    println(result.rawString)
    assert(result.blocks.size == baseCode.blocks.size)

  }

}