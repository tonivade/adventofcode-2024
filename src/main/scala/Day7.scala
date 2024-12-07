package day7

import scala.io.Source
import scala.annotation.tailrec

// https://adventofcode.com/2024/day/7
object Day7:

  case class Equation(target: Long, operands: List[Long]):
    def solve1: Boolean = calculate1(Set(operands.head), operands.tail).contains(target)
    def solve2: Boolean = calculate2(Set(operands.head), operands.tail).contains(target)
  
  extension (a: Long)
    def ++(b: Long): Long = (a.toString() + b.toString()).toLong

  @tailrec
  def calculate1(result: Set[Long], operands: List[Long]): Set[Long] =
    operands match
      case head :: tail => 
        calculate1(result.flatMap(a => List(a + head, a * head)), tail)
      case Nil => result

  @tailrec
  def calculate2(result: Set[Long], operands: List[Long]): Set[Long] =
    operands match
      case head :: tail => 
        calculate2(result.flatMap(a => List(a + head, a * head, a ++ head)), tail)
      case Nil => result

  def parse(input: String): Iterator[Equation] =
    input.linesIterator.map(_.split(": ")).map:
      case Array(number, operands) => 
        Equation(number.toLong, operands.split(" ").map(_.toLong).toList)

  def part1(input: String): Long = 
    parse(input).filter(_.solve1).map(_.target).sum

  def part2(input: String): Long = 
    parse(input).filter(_.solve2).map(_.target).sum

@main def main: Unit =
  val input = Source.fromFile("input/day7.txt").getLines().mkString("\n")
  println(Day7.part1(input))
  println(Day7.part2(input))

