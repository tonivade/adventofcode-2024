package day1

import scala.io.Source

// https://adventofcode.com/2024/day/1
object Day1:
  def parse(input: String): (List[Int], List[Int]) =
    input.linesIterator.map(_.split("   ")).foldLeft((List.empty[Int], List.empty[Int])) {
      case ((left, right), Array(l, r)) => (l.toInt :: left, r.toInt :: right)
    }

  def part1(input: String): Int = 
    val (left, right) = parse(input)
    (left.sorted zip right.sorted).map {
      case (l, r) => Math.abs(l - r)
    }.sum

  def part2(input: String): Int = 
    val (left, right) = parse(input)
    left.map {
      case l => l * right.count(_ == l)
    }.sum

@main def main: Unit =
  val input = Source.fromFile("input/day1.txt").getLines().mkString("\n")
  println(Day1.part1(input))
  println(Day1.part2(input))

