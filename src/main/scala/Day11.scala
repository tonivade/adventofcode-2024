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

  @tailrec
  def blink(stones: List[Long], step: Int): List[Long] =
    if (step > 0)
      blink(stones.flatMap(rules), step - 1)
    else
      stones

  def parse(input: String): List[Long] =
    input.split(" ").map(_.toLong).toList

  def part1(input: String): Int = 
    blink(parse(input), 25).size

  def part2(input: String): Int =
    blink(parse(input), 75).size

@main def main: Unit =
  val input = Source.fromFile("input/day11.txt").getLines().mkString("\n")
  println(Day11.part1(input))
  println(Day11.part2(input))

