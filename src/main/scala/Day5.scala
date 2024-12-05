package day5

import scala.io.Source

// https://adventofcode.com/2024/day/5
object Day5:
  def part1(input: String): Int = ???
  def part2(input: String): Int = ???

@main def main: Unit =
  val input = Source.fromFile("input/day5.txt").getLines().mkString("\n")
  println(Day5.part1(input))
  println(Day5.part2(input))

