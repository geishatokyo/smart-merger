package com.geishatokyo.smartmerger

import com.geishatokyo.smartmerger.injection.Injection
import org.scalatest.{Matchers, FlatSpec}
import com.geishatokyo.codegen.util.RichFile._
import com.geishatokyo.codegen.util.RichFile

/**
 * Created by takeshita on 2014/06/17.
 */
class UsageTest extends FlatSpec with Matchers {

  "holdMerge" should "be used as such" in {

    val f = RichFile.fromPath("src/test/resources/UsageSampleForHoldMerge.scala")
    val copied = f.copyTo("target/temp/sample1.scala")

    val newCode =
      """
        |class ChangedClass{
        |  //@hold[block1]
        |  //@end
        |}
        |
        |
        |//@hold[block2]
        |//@end
        |
        |class ChangedClass2{
        |}
        |
      """.stripMargin

    val merger = Merger.forScala
    val mergedCode = merger.holdMerge(copied,newCode)

    println(mergedCode)
    val mergedFileContent = copied.readAsString()
    assert(mergedFileContent.contains("HandMadeClass"))
    assert(mergedFileContent.contains("ChangedClass"))
    assert(mergedFileContent.contains("ChangedClass2"))
    assert(mergedFileContent == mergedCode)
  }
  "replaceMerge" should "be used as such" in {
    val f = RichFile.fromPath("src/test/resources/UsageSampleForReplaceMerge.scala")
    val copied = f.copyTo("target/temp/sample2.scala")


    val merger = Merger.forScala

    import com.geishatokyo.smartmerger.dsl.Implicits._

    val mergedCode = merger.replaceMerge(copied,
      List(
        Injection("field", "val replaced1 = 39023"),
        Injection("field", "val replaced2 = 2932") ifNotContain "replaced2",
        Injection("method","def thisIsNotInjected = {}") ifNotContain "methodAlreadyExists",
        Injection("method","def newMethod1 = {}") ifNotContain "newMethod",
        Injection("method","def newMethod2 = {}")
    ))

    println(mergedCode)
    val mergedFileContent = copied.readAsString()
    assert(mergedFileContent.contains("replaced1"))
    assert(mergedFileContent.contains("replaced2"))
    assert(mergedFileContent.contains("methodAlreadyExists"))
    assert(mergedFileContent.contains("newMethod1"))
    assert(mergedFileContent.contains("newMethod2"))

    assert(!mergedFileContent.contains("thisIsNotInjected"))
    assert(mergedFileContent == mergedCode)
  }

}
