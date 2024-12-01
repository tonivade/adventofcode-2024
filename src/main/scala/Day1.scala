package day1

import scala.io.Source

// https://adventofcode.com/2024/day/1
object Day1:
  def part1(input: String): Int = 
    val (leftResult, rightResult) = input.linesIterator.map(_.split("   ")).foldLeft((List.empty[Int], List.empty[Int])) {
      case ((leftList, rightList), Array(left, right)) => (left.toInt :: leftList, right.toInt :: rightList)
    }
    (leftResult.sorted zip rightResult.sorted).map {
      case (left, right) => Math.abs(left - right)
    }.sum

  def part2(input: String): Int = ???

@main def main: Unit =
  val input = Source.fromFile("input/day1.txt").getLines().mkString("\n")
  println(Day1.part1(input))
  println(Day1.part2(input))

