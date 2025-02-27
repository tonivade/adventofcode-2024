package day11

import scala.io.Source
import scala.util.chaining._
import scala.annotation.tailrec

// https://adventofcode.com/2024/day/11
object Day11:

  def rules(stone: Long): List[Long] =
    stone.toString() match
      case "0" => List(1)
      case x if (x.size % 2 == 0) => 
        List(x.take(x.size / 2).toLong, x.drop(x.size / 2).toLong)
      case x => List(stone * 2024L)

  extension (stones: Map[Long, Long])
    @tailrec
    def blink(step: Int): Map[Long, Long] =
      if (step > 0)
        val next = stones.iterator.flatMap:
          case (stone, count) => rules(stone).map(_ -> count)
        .toList
        next.groupMapReduce(_._1)(_._2)(_ + _).blink(step - 1)
      else
        stones

  def parse(input: String): Map[Long, Long] =
    input.split(" ").map(_.toLong -> 1L).toMap

  def part1(input: String): Long = 
    parse(input).blink(25).values.sum

  def part2(input: String): Long =
    parse(input).blink(75).values.sum

@main def main: Unit =
  val input = Source.fromFile("input/day11.txt").getLines().mkString("\n")
  println(Day11.part1(input))
  println(Day11.part2(input))

