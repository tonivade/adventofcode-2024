package day2

import scala.io.Source

// https://adventofcode.com/2024/day/2
object Day2:

  def parse(input: String): Iterator[Array[Int]] =
    input.linesIterator.map(_.split(" ")).map(_.map(_.toInt))

  def isSafe(report: Array[Int]): Boolean =
    val diffs = report.sliding(2).map {
      case Array(a, b) => a - b
    }.toList
    diffs.forall(Math.abs(_) <= 3) && (diffs.forall(_ > 0) || diffs.forall(_ < 0))

  def generateAll(report: Array[Int]): Array[Array[Int]] =
    report.zipWithIndex.map {
      case (_, i) => report.take(i) ++ report.drop(i + 1)
    }

  def isSafeWithTolerance(report: Array[Int]): Boolean =
    isSafe(report) || generateAll(report).exists(isSafe)

  def part1(input: String): Int = 
    parse(input)
      .filter(isSafe)
      .size

  def part2(input: String): Int = 
    parse(input)
      .filter(isSafeWithTolerance)
      .size

@main def main: Unit =
  val input = Source.fromFile("input/day2.txt").getLines().mkString("\n")
  println(Day2.part1(input))
  println(Day2.part2(input))

