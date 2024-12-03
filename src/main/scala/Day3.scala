package day3

import scala.io.Source

// https://adventofcode.com/2024/day/3
object Day3:
  def part1(input: String): Int = 
    val regex = """mul\((\d+),(\d+)\)""".r
    regex.findAllMatchIn(input).map:
      mul => mul.group(1).toInt * mul.group(2).toInt
    .sum

  def part2(input: String): Int = ???

@main def main: Unit =
  val input = Source.fromFile("input/day3.txt").getLines().mkString("\n")
  println(Day3.part1(input))
  println(Day3.part2(input))

