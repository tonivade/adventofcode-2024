package day3

import scala.io.Source

// https://adventofcode.com/2024/day/3
object Day3:
  def part1(input: String): Int = 
    val regex = """mul\((\d+),(\d+)\)""".r
    regex.findAllMatchIn(input).map:
      m => m.group(1).toInt * m.group(2).toInt
    .sum

  def part2(input: String): Int = 
    val regex = """do(?:n't)?\(\)|mul\((\d+),(\d+)\)""".r
    regex.findAllMatchIn(input).foldLeft((true, 0)):
      case ((enabled, result), m) =>
        m.group(0) match
          case "do()" => (true, result)
          case "don't()" => (false, result)
          case _ if enabled => (enabled, result + (m.group(1).toInt * m.group(2).toInt))
          case _ => (enabled, result)
    ._2

@main def main: Unit =
  val input = Source.fromFile("input/day3.txt").getLines().mkString("\n")
  println(Day3.part1(input))
  println(Day3.part2(input))

