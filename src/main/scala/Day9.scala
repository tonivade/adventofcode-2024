package day9

import scala.io.Source
import scala.util.chaining._
import scala.annotation.tailrec
import scala.collection.mutable.Buffer

// https://adventofcode.com/2024/day/9
object Day9:

  enum Sector:
    case File(id: Long)
    case Free
    def repeat(times: Int): List[Sector] = 
      if (times == 0)
        Nil
      else
        (1 to times).map(_ => this).toList

  import Sector._

  def expand(input: String): Buffer[Sector] = 
    input.zipWithIndex.foldLeft(0L, Buffer.empty[Sector]):
      case ((fileId, result), (ch, i)) =>
        if (i % 2 == 0)
          (fileId + 1, result ++ File(fileId).repeat(ch.toInt - 48))
        else
          (fileId, result ++ Free.repeat(ch.toInt - 48))
    ._2

  @tailrec
  def compact1(input: Buffer[Sector], start: Int = 0, end: Int = 0): Buffer[Sector] = 
    val freePosition = input.iterator.indexWhere(_ == Free, start)
    val lastPosition = input.reverseIterator.indexWhere(_ != Free, end)

    val position = input.size - lastPosition - 1
    if (freePosition > position)
      input
    else
      input(freePosition) = input(position)
      input(position) = Free
      compact1(input, freePosition, lastPosition)
  
  def compact2(input: Buffer[Sector], start: Int = 0, end: Int = 0): Buffer[Sector] = ???

  def checksum(input: Iterable[Sector]): Long = 
    input.zipWithIndex.foldLeft(0L):
      case (sum, (File(id), i)) => sum + (id * i)
      case (sum, _) => sum

  def part1(input: String): Long = 
    val expanded = expand(input)
    val compacted = compact1(expanded)
    checksum(compacted)

  def part2(input: String): Long = 
    val expanded = expand(input)
    val compacted = compact2(expanded)
    checksum(compacted)

@main def main: Unit =
  val input = Source.fromFile("input/day9.txt").getLines().mkString("\n")
  println(Day9.part1(input))
  println(Day9.part2(input))
