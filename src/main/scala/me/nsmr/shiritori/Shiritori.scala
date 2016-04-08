package me.nsmr.shiritori

object Shiritori {
  def words: Set[Symbol] = ???
}

class Shiritori {
  private[this] var _stock = Shiritori.words

  def stock = _stock

  def seek(word: String): String = {
    stock.find {
      case Symbol(next) => next.head == word.last
    } match {
      case Some(Symbol(next)) =>
        if (!next.endsWith("ん") && !next.endsWith("ン")) word
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
