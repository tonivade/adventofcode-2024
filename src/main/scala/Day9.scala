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
  def compact(input: List[Sector]): List[Sector] = 
    val freePosition = input.zipWithIndex.find(_._1.isFree).map(_._2).get
    val lastSector = input.zipWithIndex.findLast(!_._1.isFree).map(_._2).get

    if (freePosition > lastSector)
      input
    else
      val buffer = input.toBuffer
      buffer(freePosition) = buffer(lastSector)
      buffer(lastSector) = Free
      compact(buffer.toList)

  def checksum(input: List[Sector]): Long = 
    input.zipWithIndex.map:
      case (File(id), i) => id * i
      case _ => 0
    .sum

  def part1(input: String): Long = 
    input pipe expand pipe compact pipe checksum

  def part2(input: String): Int = ???

@main def main: Unit =
  val input = Source.fromFile("input/day9.txt").getLines().mkString("\n")
  println(Day9.part1(input))
  println(Day9.part2(input))

