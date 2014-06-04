package com.geishatokyo.codegen.util

import java.io.{FileOutputStream, File, FileInputStream}

/**
 * 
 * User: takeshita
 * DateTime: 13/09/09 17:33
 */
object FileUtil {

  def read(filename : String) = {
    val input = new FileInputStream(filename)
    val d = new Array[Byte](input.available())
    input.read(d)
    input.close()
    new String(d,"utf-8")
  }

  def write(filename : String,content : String) : Unit = {
    if (content == null || content.length == 0) return
    val f = new File(filename).getParentFile
    f.mkdirs()
    val output = new FileOutputStream(filename)
    output.write(content.getBytes("utf-8"))

    output.close()
  }
}
