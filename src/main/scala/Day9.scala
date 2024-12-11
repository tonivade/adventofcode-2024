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
    val reversePosition = input.reverseIterator.indexWhere(_ != Free, end)

    val filePosition = input.size - reversePosition - 1
    if (freePosition > filePosition)
      input
    else
      input(freePosition) = input(filePosition)
      input(filePosition) = Free
      compact1(input, freePosition, reversePosition)
  
  @tailrec
  def compact2(input: Buffer[Sector], start: Int = 0, end: Int = 0, processed: Set[Sector] = Set(Free)): Buffer[Sector] =
    if (start >= input.size - end)
      input
    else
      val start1 = System.nanoTime()
      // find first non free sector starting the from end
      val reversePosition = input.reverseIterator.indexWhere(!processed.contains(_), end)
      val filePosition = input.size - reversePosition - 1
      val file = input(filePosition)
      // calculate the size of the file
      val fileSize = input.reverseIterator.drop(reversePosition).takeWhile(_ == file).size
      val end1 = System.nanoTime() - start1

      val start2 = System.nanoTime()
      // find first free slot to put the file
      val freePosition = input.take(filePosition - fileSize).indexOfSlice(Free.repeat(fileSize), start)
      val end2 = System.nanoTime() - start2

      val start3 = System.nanoTime()
      if (freePosition > -1)
        for (i <- 0 until fileSize) 
          input(freePosition + i) = input(filePosition - i)
          input(filePosition - i) = Free
      val end3 = System.nanoTime() - start3

      println(s"${file} file: $end1, free: $end2, swap: $end3, total: ${System.nanoTime() - start1}")
      
      compact2(input, input.indexWhere(_ == Free, start), reversePosition + fileSize, processed + file)

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

