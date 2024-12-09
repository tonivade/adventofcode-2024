package day9

import scala.io.Source
import scala.util.chaining._
import scala.annotation.tailrec

// https://adventofcode.com/2024/day/9
object Day9:

  enum Sector:
    case File(id: Long)
    case Free
    def isFree: Boolean = this match
      case Free => true
      case _ => false
    def repeat(times: Int): List[Sector] = 
      if (times == 0)
        Nil
      else
        (1 to times).map(_ => this).toList

  import Sector._

  def expand(input: String): List[Sector] = 
    input.zipWithIndex.foldLeft(0L, List.empty[Sector]):
      case ((fileId, result), (ch, i)) =>
        if (i % 2 == 0)
          (fileId + 1, result ++ File(fileId).repeat(ch.toInt - 48))
        else
          (fileId, result ++ Free.repeat(ch.toInt - 48))
    ._2

  @tailrec
  def compact(input: List[Sector], start: Int = 0, end: Int = 0): List[Sector] = 
    val (_, freePosition) = input.iterator.zipWithIndex.drop(start).find(_._1.isFree).get
    val (lastSector, lastPosition) = input.reverseIterator.zipWithIndex.drop(end).find(!_._1.isFree).get

    if (freePosition > (input.size - lastPosition - 1))
      input
    else
      val buffer = input.toBuffer
      buffer(freePosition) = lastSector
      buffer(input.size - lastPosition - 1) = Free
      compact(buffer.toList, freePosition, lastPosition)

  def checksum(input: List[Sector]): Long = 
    input.zipWithIndex.foldLeft(0L):
      case (sum, (File(id), i)) => sum + (id * i)
      case (sum, _) => sum

  def part1(input: String): Long = 
    val expanded = expand(input)
    val compacted = compact(expanded)
    checksum(compacted)

  def part2(input: String): Int = ???

@main def main: Unit =
  val input = Source.fromFile("input/day9.txt").getLines().mkString("\n")
  println(Day9.part1(input))
  println(Day9.part2(input))

