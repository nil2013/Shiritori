package me.nsmr

import java.io.BufferedReader
import java.io.BufferedWriter
import java.io.File
import java.io.FileInputStream
import java.io.FileWriter
import java.io.InputStreamReader

object `package` {
  
  def itself[A]: A => A = (it => it)

  def using[A, R <: { def close() }](r : R)(body : R => A) : A = try { body(r) } finally { r.close() }

  implicit class IterableBufferedReader(br: java.io.BufferedReader) extends Iterable[String] {
    def iterator: Iterator[String] = Iterator.continually(br.readLine()).takeWhile(_ != null)
  }

  object FileUtil {
    def reading[A](path: String)(body: Iterator[String] => A): A = reading(new File(path))(body)
  
    def reading[A](file: File)(body: Iterator[String] => A): A = using(new BufferedReader(new InputStreamReader(new FileInputStream(file)))) { br =>
      body(br.iterator)
    }
  
    def writing[A](path: String)(body: BufferedWriter => A): A = writing(new File(path))(body)
  
    def writing[A](file: File)(body: BufferedWriter => A): A = using(new BufferedWriter(new FileWriter(file)))(body)
  }

  sealed trait Colors {
    def ansi: String
    def start = print(ansi)
    def reset = Colors.Reset.start
    def using[A](body: => A): A = coloring(this)(body)
  }
  object Colors {
    case object Reset extends Colors { def ansi = "\u001b[00m" }
    case object Red extends Colors { def ansi = "\u001b[31m" }
    case object Green extends Colors { def ansi = "\u001b[32m" }
    case object Yellow extends Colors { def ansi = "\u001b[33m" }
    case object Blue extends Colors { def ansi = "\u001b[34m" }
    case object Purple extends Colors { def ansi = "\u001b[35m" }
    case object LightBlue extends Colors { def ansi = "\u001b[36m" }
    case object White extends Colors { def ansi = "\u001b[37m" }
    case object RollingOver extends Colors { def ansi = "\u001b[7m" }
  }
  def coloring[A](color: Colors)(body: => A) = try { color.start; body } finally { color.reset }
}