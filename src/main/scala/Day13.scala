package day13

import scala.io.Source

// https://adventofcode.com/2024/day/13
object Day13:

  case class Button(x: Int, y: Int)
  case class Prize(x: Int, y: Int)
  case class Machine(a: Button, b: Button, prize: Prize):
    def cost: Option[Int] = ???

  val buttonRegex = """Button (A|B): X\+(\d+), Y\+(\d+)""".r
  val prizeRegex = """Prize: X=(\d+), Y=(\d+)""".r

  def parseMachine(input: String): Machine = 
    input.split("\n") match
      case Array(a, b, p) =>
        val buttonA = a match
          case buttonRegex(_, x, y) => Button(x.toInt, y.toInt)
        val buttonB = b match
          case buttonRegex(_, x, y) => Button(x.toInt, y.toInt)
        val prize = p match
          case prizeRegex(x, y) => Prize(x.toInt, y.toInt)
        Machine(buttonA, buttonB, prize)

  def parse(input: String): Set[Machine] = 
    input.split("\n\n").map(parseMachine).toSet

  def part1(input: String): Int = 
    parse(input).map(_.cost).filter(_.isDefined).map(_.get).sum
    
  def part2(input: String): Int = ???

@main def main: Unit =
  val input = Source.fromFile("input/day13.txt").getLines().mkString("\n")
  println(Day13.part1(input))
  println(Day13.part2(input))

