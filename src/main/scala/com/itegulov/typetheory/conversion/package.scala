package com.itegulov.typetheory

import com.itegulov.typetheory.debruijn._
import com.itegulov.typetheory.lambda._

import scala.annotation.tailrec

/**
 * @author Daniyar Itegulov
 */
package object conversion {
  private val encode = collection.mutable.Map[Char, Int]()
  private val decode = collection.mutable.Map[Int, Char]()
  for (char <- 'a' to 'z') {
    encode(char) = char - 'a'
    decode(char - 'a') = char
  }
  for (char <- '0' to '9') {
    encode(char) = 26 + char - '0'
    decode(26 + char - '0') = char
  }
  encode('\'') = 36
  decode(36) = '\''

  @tailrec
  private def rename(result: Int, string: String): Int = {
    if (string.length == 1) 37 * result + encode(string.head)
    else rename(37 * result + encode(string.head), string.tail)
  }

  private def rename(string: String): Int = {
    rename(0, string)
  }

  @tailrec
  private def renameBack(result: StringBuilder, hash: Int): Unit = {
    if (hash != 0) {
      result.append(decode(hash % 37))
      renameBack(result, hash / 37)
    }
  }

  private def renameBack(hash: Int): String = {
    val sb = new StringBuilder
    renameBack(sb, hash)
    sb.toString()
  }

  private def addNames(exp: DeBruijn): Lambda = ???

  private def removeNames(exp: Lambda, map: Map[String, Int], depth: Int): DeBruijn = {
    exp match {
      case Var(name) => if (map.contains(name)) DVar(depth - map(name)) else DVar(depth + rename(name) + 1)
      case App(l, r) => DApp(removeNames(l, map, depth), removeNames(r, map, depth))
      case Abs(v, e) => DAbs(removeNames(e, map + (v.name -> depth), depth + 1))
    }
  }

  def removeNames(exp: Lambda): DeBruijn = {
    removeNames(exp, Map(), 0)
  }
}
