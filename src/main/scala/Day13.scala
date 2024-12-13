package day13

import scala.io.Source

// https://adventofcode.com/2024/day/13
object Day13:

  case class Button(x: Int, y: Int, cost: Int)
  case class Prize(x: Int, y: Int)
  case class Machine(buttonA: Button, buttonB: Button, prize: Prize):
    def cost: Option[Int] = 
      val aCoeff = buttonA.x * buttonB.y - buttonA.y * buttonB.x
      val rhs = prize.x * buttonB.y - prize.y * buttonB.x
      if (rhs % aCoeff == 0)
        val a = rhs / aCoeff
        val b = (prize.x - buttonA.x * a) / buttonB.x
        Some(a * buttonA.cost + b * buttonB.cost)
      else
        None

  val buttonRegex = """Button (?:A|B): X\+(\d+), Y\+(\d+)""".r
  val prizeRegex = """Prize: X=(\d+), Y=(\d+)""".r

  def parseMachine(input: String): Machine = 
    input.split("\n") match
      case Array(a, b, p) =>
        val buttonA = a match
          case buttonRegex(x, y) => Button(x.toInt, y.toInt, 3)
        val buttonB = b match
          case buttonRegex(x, y) => Button(x.toInt, y.toInt, 1)
        val prize = p match
          case prizeRegex(x, y) => Prize(x.toInt, y.toInt)
        Machine(buttonA, buttonB, prize)

  def parse(input: String): List[Machine] =
    input.split("\n\n").map(parseMachine).toList

  def part1(input: String): Int = 
    parse(input).map(_.cost).filter(_.isDefined).map(_.get).sum

  def part2(input: String): Int = ???

@main def main: Unit =
  val input = Source.fromFile("input/day13.txt").getLines().mkString("\n")
  println(Day13.part1(input))
  println(Day13.part2(input))

