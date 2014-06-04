package com.geishatokyo.smartmerger.util

import org.scalatest.{Matchers, FlatSpec}
import org.scalatest.matchers.ShouldMatchers

/**
 * Created by takeshita on 2014/06/03.
 */
class TrieMapTest extends FlatSpec with Matchers {

  "add" should "add new node" in {

    val map = new TrieMap[String]

    map += ("a" -> "hoge")
    map += ("ab" -> "fuga")
    map += ("abc" -> "fuga2")
    map += ("bc" -> "wahoo")

    println(map.asTreeString())

    assert(map.depth == 3)
    assert(map("a") == "hoge")
    assert(map("ab") == "fuga")
    assert(map("abc") == "fuga2")
    assert(map("bc") == "wahoo")

  }
  "add" should "update old key" in {
    val map = new TrieMap[String]
    map += ("abc" -> "hoge")
    map += ("addd" -> "hoge")

    map += ("addd" -> "wahoo")

    assert(map.depth == 4)
    assert(map("abc") == "hoge")
    assert(map("addd") == "wahoo")
  }

  "get" should "match perfectly" in {

    val map = new TrieMap[String]
    map += ("abc" -> "hoge")

    assert(map("abc") == "hoge")
    assert(map.get("ab") == None)
    assert(map.get("abcd") == None)
  }

  "getLongest" should "match longest value" in {

    val map = new TrieMap[String]
    map += ("aaa" -> "a3")
    map += ("aaaa" -> "a4")
    map += ("aaaaa" -> "a5")

    assert(map.getLongest("aaa") == Some("a3"))
    assert(map.getLongest("aaaa") == Some("a4"))
    assert(map.getLongest("aaaaa") == Some("a5"))
    assert(map.getLongest("aaaaaa") == Some("a5"))
    assert(map.getLongest("aaabbb") == Some("a3"))
    assert(map.getLongest("aaaabb") == Some("a4"))
    assert(map.getLongest("aaaaab") == Some("a5"))

  }

  "getShortest" should "match shortest value" in {

    val map = new TrieMap[String]
    map += ("aaa" -> "a3")
    map += ("aaaa" -> "a4")
    map += ("aaaaa" -> "a5")

    assert(map.getShortest("aaa") == Some("a3"))
    assert(map.getShortest("aaaa") == Some("a3"))
    assert(map.getShortest("aaaaa") == Some("a3"))
    assert(map.getShortest("aaaaaa") == Some("a3"))
    assert(map.getShortest("aaabbb") == Some("a3"))
    assert(map.getShortest("aaaabb") == Some("a3"))
    assert(map.getShortest("aaaaab") == Some("a3"))

  }


}