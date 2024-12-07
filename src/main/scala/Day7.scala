package day7

import scala.io.Source
import scala.annotation.tailrec

// https://adventofcode.com/2024/day/7
object Day7:

  case class Equation(result: Long, operands: List[Long]):
    def solve: Boolean = calculate(List(1), operands).contains(result)

  @tailrec
  def calculate(result: List[Long], operands: List[Long]): List[Long] =
    operands match
      case head :: tail => calculate(result.flatMap(a => List(a + head, a * head)), tail)
      case Nil => result

  def part1(input: String): Long = 
    val result = input.linesIterator.map(_.split(": ")).map:
      case Array(number, operands) => 
        Equation(number.toLong, operands.split(" ").map(_.toLong).toList)
    result.filter(_.solve).map(_.result).sum

  def part2(input: String): Int = ???

@main def main: Unit =
  val input = Source.fromFile("input/day7.txt").getLines().mkString("\n")
  println(Day7.part1(input))
  println(Day7.part2(input))

