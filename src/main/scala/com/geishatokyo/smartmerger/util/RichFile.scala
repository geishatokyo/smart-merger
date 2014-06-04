package com.geishatokyo.codegen.util

import java.io.{FileOutputStream, FileInputStream, FileNotFoundException, File}

/**
 * 
 * User: takeshita
 * DateTime: 13/06/19 15:17
 */
object RichFile{

  implicit def fromFile(file : File) = new RichFile(file)
  implicit def fromString(file : String) : RichFile = {
    val f = new File(file)
    if (f.exists()) return new RichFile(f)

    val r = getClass.getClassLoader.getResource(file)
    if (r != null){
      val f2 = new File(r.getFile)
      if (f2.exists()){
        return new RichFile(f2)
      }
    }

    val r2 = getClass.getResource(file)
    if (r2 != null){
      val f3 = new File(r2.getFile)
      if (f3.exists()){
        return new RichFile(f3)
      }
    }

    new RichFile(f)


  }
  implicit def asFile( f : RichFile) = f.asFile

}

class RichFile(file : File) {

  def asFile : File = file

  def read() : Array[Byte] = {
    val input = new FileInputStream(asFile)
    try{
      val data = new Array[Byte](input.available())
      input.read(data)
      data
    }finally{
      input.close()
    }
  }
  def readAsString() : String = {
    new String(read(),"utf-8")
  }

  def write(bytes : Array[Byte]) : File = {
    val f = asFile

    if (!f.getParentFile.exists()){
      Logger.log("Make parent dirs")
      f.getParentFile.mkdirs()
    }

    val output = new FileOutputStream(f)
    try{
      output.write(bytes)
      Logger.log("Write to " + f.getAbsolutePath)
    }finally{
      output.close()
    }
    f
  }

  def write(str : String) : File = write(str.getBytes("UTF-8"))

  def copyFrom(file : File) : File = {
    val rf = new RichFile(file)
    val d  =rf.read()
    write(d)

  }

  def copyFrom( file : String) : File = {
    copyFrom(RichFile.fromString(file))
  }

  def copyTo(file : String) : File = copyTo(RichFile.fromString(file))
  def copyTo(file : File) : File = {
    val rf = new RichFile(file)
    if (this.asFile.isDirectory) return null
    val d = read()
    rf.write(d)
  }

  def delete() = {
    val f = asFile
    Logger.log("Delete " + f.getAbsolutePath)
    f.delete()
  }

  def deleteDir() = {
    def _deleteDir(dir : File) : Unit ={
      if (dir.listFiles() == null) return;
      dir.listFiles().foreach(d => {
        if (d.isDirectory) _deleteDir(d)
        else d.delete()
      })
    }
    val dir = asFile
    if (dir.isFile){
      Logger.log("Delete " + dir.getAbsolutePath)
      dir.delete()
    }else{
      Logger.log("Delete dir " + dir.getAbsolutePath)
      _deleteDir(dir)
      dir.delete()
    }
  }

  def copyAllFiles(destDir : File) = {

    val dir = asFile
    list().foreach(f => {
      val dest = new File(destDir,f)
      RichFile.fromFile(new File(dir,f)).copyTo(dest)

    })

  }

  def /( path : String) = new RichFile(new File(file,path))

  def list() = {
    val dir = asFile
    val l = asFile.list()
    if(l == null) Array[String]()
    else l.filter(f => {
      val t =new File(dir,f)
      t.isFile
    })
  }

}
