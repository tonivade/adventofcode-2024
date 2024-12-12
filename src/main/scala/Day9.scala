package day9

import scala.io.Source
import scala.annotation.tailrec
import scala.collection.mutable.Buffer
import day9.Day9.Sector.File

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

  enum Block:
    case FileBlock(id: Long, start: Int, size: Int)
    case FreeBlock(start: Int, size: Int)
    def start: Int
    def sectors: List[Sector] =
      this match
        case FileBlock(id, _, size) => File(id).repeat(size)
        case FreeBlock(_, size) => Free.repeat(size)
      
  import Block._

  def expand1(input: String): Buffer[Sector] = 
    input.zipWithIndex.foldLeft(0L, Buffer.empty[Sector]):
      case ((fileId, result), (ch, i)) =>
        if (i % 2 == 0)
          (fileId + 1, result ++ File(fileId).repeat(ch.toInt - 48))
        else
          (fileId, result ++ Free.repeat(ch.toInt - 48))
    ._2

  def expand2(input: String): (Buffer[FreeBlock], Buffer[FileBlock]) = 
    input.zipWithIndex.foldLeft(0L, 0, (Buffer.empty[FreeBlock], Buffer.empty[FileBlock])):
      case ((fileId, position, (freeBlocks, fileBlocks)), (ch, i)) =>
        val size = ch.toInt - 48
        if (i % 2 == 0)
          (fileId + 1, position + size, (freeBlocks, fileBlocks :+ FileBlock(fileId, position, size)))
        else
          (fileId, position + size, (freeBlocks :+ FreeBlock(position, size), fileBlocks))
    ._3

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
  def compact2(freeBlocks: Buffer[FreeBlock], fileBlocks: Buffer[FileBlock], current: Int = 0): Buffer[Sector] =
    if (current >= fileBlocks.size)
      (fileBlocks ++ freeBlocks).sortBy(_.start).flatMap(_.sectors)
    else
      val file = fileBlocks(current)
      val freePosition = freeBlocks.indexWhere(free => free.start < file.start && free.size >= file.size)
      
      if (freePosition > -1)
        val free = freeBlocks(freePosition)
        fileBlocks(current) = FileBlock(file.id, free.start, file.size)
        freeBlocks(freePosition) = FreeBlock(free.start + file.size, free.size - file.size)
        freeBlocks.addOne(FreeBlock(file.start, file.size))
      
      compact2(freeBlocks, fileBlocks, current + 1)

  def checksum(input: Iterable[Sector]): Long = 
    input.zipWithIndex.foldLeft(0L):
      case (sum, (File(id), i)) => sum + (id * i)
      case (sum, _) => sum

  def part1(input: String): Long = 
    val expanded = expand1(input)
    val compacted = compact1(expanded)
    checksum(compacted)

  def part2(input: String): Long = 
    val (freeBlocks, fileBlocks) = expand2(input)
    val compacted = compact2(freeBlocks, fileBlocks.reverse)
    checksum(compacted)

@main def main: Unit =
  val input = Source.fromFile("input/day9.txt").getLines().mkString("\n")
  println(Day9.part1(input))
  println(Day9.part2(input))

