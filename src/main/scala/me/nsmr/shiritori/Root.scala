package me.nsmr
package shiritori

object Root {
  def main(args: Array[String]) {
    val game = new Game
    System.out.println("ニューゲームです。")
    game.play match {
      case (true, cnt)  => println(s"${cnt}語であなたの勝利です。")
      case (false, cnt) => println(s"${cnt}語であなたの敗北です。")
    }
  }
}

object Game {
  final val pat = """[ぁ-んァ-ン]+""".r
}
class Game {
  import Game._
  private var _context: Shiritori = new Shiritori
  def context = _context
  private var last = "しりとり"

  def reset = {
    _context = new Shiritori
  }

  def play: (Boolean, Int) = {
    var cnt = 0
    println("しりとり")
    context.consume("しりとり")
    print("> ")
    try {
      scala.io.Source.stdin.getLines().takeWhile {
        case "あきた" => false
        case word if word.endsWith("ん") || word.endsWith("ン") =>
          println("末尾が「ん」で終わる単語を入力しましたね。")
          false
        case word if word.head != last.last =>
          println("前の文字とつながっていません！")
          false
        case _ => true
      }.foreach {
        case word @ pat() if word.head == last.last =>
          context.consume(word.toHiragana)
          val ans = context.seek(word.toHiragana)
          last = ans
          context.consume(ans)
          cnt = cnt + 1
          println(ans)
          print("> ")
          true
        case _ =>
          System.err.println("ひらがな又はカタカナだけで構成された文字列を入力してください...")
          true
      }
      (false, cnt)
    } catch {
      case e: WordNotFoundException =>
        System.out.println("単語が見つかりません。。。")
        (true, cnt)
    }
  }
}
