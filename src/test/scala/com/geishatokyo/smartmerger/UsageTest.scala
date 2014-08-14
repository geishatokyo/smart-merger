package com.geishatokyo.smartmerger

import org.scalatest.{Matchers, FlatSpec}
import com.geishatokyo.codegen.util.RichFile._
import com.geishatokyo.codegen.util.RichFile
import com.geishatokyo.smartmerger.injection.{ReplaceInjection, InjectionData}

/**
 * Created by takeshita on 2014/06/17.
 */
class UsageTest extends FlatSpec with Matchers {

  "holdMerge" should "be used as such" in {

    val f = RichFile.fromString("src/test/resources/UsageSampleForHoldMerge.scala")
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
    val f = RichFile.fromString("src/test/resources/UsageSampleForReplaceMerge.scala")
    val copied = f.copyTo("target/temp/sample2.scala")


    val merger = Merger.forScala

    import com.geishatokyo.smartmerger.dsl.Implicits._

    val mergedCode = merger.replaceMerge(copied,InjectionData(
      replace("field") to "val replaced1 = 39023",
      replace("field") to "val replaced2 = 2932" ifNotContains "replaced2",
      replace("method") to "def thisIsNotInjected = {}" ifNotContains "methodAlreadyExists",
      replace("method") to "def newMethod1 = {}" ifNotContains "newMethod",
      replace("method") to "def newMethod2 = {}"
    ),"This code is used instead of file content if passed file doesn't exists.")

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
