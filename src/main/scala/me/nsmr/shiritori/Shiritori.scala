package me.nsmr
package shiritori

import java.io.File

object Shiritori {
  // this.getClass.getClassLoader.getResourceAsStream("")
  def loading[A](body: => A): A = {
    try {
      System.out.println("Loading...")
      body
    } finally {
      println
      System.out.println("Load finished!")
    }
  }
  def words: Set[Symbol] = {
    FileUtil.reading(new File("resources/jawiki-latest-all-titles")) { lines =>
      val pat = """[ぁ-んァ-ン]+""".r
      var cnt = 0
      loading {
        lines.collect {
          case line @ pat() =>
            cnt = cnt + 1
            print(s"\rloaded: ${cnt}")
            Symbol(line.toHiragana)
        }.toSet
      }
    }
  }
}

class Shiritori {
  private[this] var _stock = Shiritori.words

  def stock = _stock

  def seek(word: String): String = {
    val initial = word.last
    stock.find {
      case Symbol(next) => next.head == initial
    } match {
      case Some(Symbol(next)) =>
        if (!next.endsWith("ん") && !next.endsWith("ン")) next
        else {
          consume(next)
          seek(word)
        }
      case None => throw WordNotFoundException(s"'${word.last}'から始まる単語はもう残っていません。。。")
    }
  }

  def consume(word: String) { _stock = stock - Symbol(word) }
}

case class WordNotFoundException(mes: String) extends Exception(mes)
