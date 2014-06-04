package com.geishatokyo.smartmerger.util

import scala.annotation.tailrec
import scala.collection.mutable

/**
 * Created by takeshita on 2014/06/03.
 */
object TrieMap{

  def apply[T](kvs : (String,T)*) : TrieMap[T] = {
    val m = new TrieMap[T]
    kvs.foreach(m += _)
    m
  }
}

class TrieMap[T] extends mutable.Map[String,T] {

  private var rootNode : Node[T] = Node('\0')

  private var _depth = 0
  def depth = _depth

  override def -=(key: String) = {
    val (index,node) = getNode(rootNode,key,0)
    if(index == key.length) {
      node.value = None
    }
    rootNode.clean
    _depth = rootNode.depth - 1
    this
  }

  override def +=(kv: (String, T)) = {

    val key = kv._1
    val v : T = kv._2.asInstanceOf[T]
    val (index,node) = getNode(rootNode,key,0)
    if(index == key.length){
      node.value = Some(v)
    }else{
      val newNode = createNode[T](key.substring(index),v)
      node.children = (newNode.c,newNode) :: node.children
    }
    _depth = rootNode.depth - 1
    this
  }

  override def iterator: Iterator[(String, T)] = ???

  override def get(key: String): Option[T] = {
    getMatchNode(rootNode,key,0)
  }

  def getLongest(text : String) : Option[T] = {
    getLongestMatchNode(rootNode,text,0,None)
  }

  def getShortest(text : String) : Option[T] = {
    getShortestMatchNode(rootNode,text,0)
  }

  def asTreeString() : String = {
    rootNode.asTreeString
  }


  private def createNode[T](key : String,value : T) = {
    val root = Node[T](key.charAt(0))
    var n = root
    for(c <- key.substring(1)){
      val n2 = Node[T](c)
      n.children = (c,n2) :: n.children
      n = n2
    }
    n.value = Some(value)
    root
  }

  @tailrec
  private def getNode(node : Node[T],key : String,index : Int) : (Int,Node[T]) = {

    if(index < key.length) {
      val c = key.charAt(index)
      node.children.find(_._1 == c) match {
        case Some((_, next)) => {
          getNode(next, key, index + 1)
        }
        case None => {
          (index,node)
        }
      }
    }else{
      (index,node)
    }
  }

  @tailrec
  private def getMatchNode(node : Node[T],full : String,index : Int) : Option[T] = {
    if(full.length <= index) {
      node.value
    }else {
      val c = full.charAt(index)
      node.children.find(_._1 == c) match{
        case Some((_,next)) => {
          getMatchNode(next, full, index + 1)
        }
        case None => {
          None
        }
      }
    }
  }
  @tailrec
  private def getShortestMatchNode(node : Node[T],full : String,index : Int) : Option[T] = {
    if(full.length <= index) {
      node.value
    }else if(node.value.isDefined){
      node.value
    }else{
      val c = full.charAt(index)
      node.children.find(_._1 == c) match{
        case Some((_,next)) => {
          getShortestMatchNode(next, full, index + 1)
        }
        case None => {
          None
        }
      }
    }
  }

  @tailrec
  private def getLongestMatchNode(node : Node[T],full : String,index : Int,candidate : Option[T]) : Option[T] = {
    if(full.length <= index) {
      node.value
    }else if(node.value.isDefined){
      val c = full.charAt(index)
      node.children.find(_._1 == c) match{
        case Some((_,next)) => {
          getLongestMatchNode(next, full, index + 1,node.value)
        }
        case None => {
          node.value
        }
      }
    }else{
      val c = full.charAt(index)
      node.children.find(_._1 == c) match{
        case Some((_,next)) => {
          getLongestMatchNode(next, full, index + 1,candidate)
        }
        case None => {
          None
        }
      }
    }
  }
  case class Node[T](c : Char){
    var value : Option[T] = None
    var children : List[(Char,Node[T])] = Nil

    def depth : Int = {
      if(children.size == 0) 1
      else children.map(_._2.depth).max + 1
    }
    def clean : Boolean = {
      val baseCount = if(value.isDefined) 1
      else 0
      children = children.filter(!_._2.clean)
      children.size + baseCount == 0
    }

    def asTreeString : String = {
      s"${c} - (${children.map(_._2.asTreeString).mkString(", ")})"
    }

  }

}
