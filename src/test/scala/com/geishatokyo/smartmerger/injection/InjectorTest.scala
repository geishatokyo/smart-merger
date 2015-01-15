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

    val mergeData = List(
        Injection("rep1","replaced!"),
        Injection("","anonymous is replaced!"),
        Injection("rep1","replaced 2!"),
        Injection("ins1","inserted!"),
        Injection("ins1","def hoge() = { this is not inserted.Because regex code already exists.}") ifNotMatch("""def\s+hoge\(\)""".r),
        Injection("ins1","def fuga() = this is inserted.") ifNotMatch("""def\s+fuga\(\)""".r),
        Injection("ins2","def hoge2() = this is inserted.") ifNotMatch("""def\s+hoge\(\)""".r)
    )

    val result = merger.inject(parsedData,mergeData)
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

    val result = merger.inject(mergeCode,baseCode)

    println(result.rawString)
    assert(result.blocks.size == baseCode.blocks.size)
    assert(result.blocks(0) == mergeCode.blocks(0))
    assert(result.blocks(1) == baseCode.blocks(1))

  }

}