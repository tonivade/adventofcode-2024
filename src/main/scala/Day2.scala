package day2

import scala.io.Source

// https://adventofcode.com/2024/day/2
object Day2:

  def parse(input: String): Iterator[List[Int]] =
    input.linesIterator.map(_.split(" ")).map {
      line => line.map(_.toInt).sliding(2).map {
        case Array(a, b) => a - b
      }.toList
    }

  def part1(input: String): Int = 
    parse(input)
      .filterNot(report => report.exists(Math.abs(_) > 3))
      .filter(report => report.forall(_ > 0) || report.forall(_ < 0))
      .size

  def part2(input: String): Int = ???

@main def main: Unit =
  val input = Source.fromFile("input/day2.txt").getLines().mkString("\n")
  println(Day2.part1(input))
  println(Day2.part2(input))

