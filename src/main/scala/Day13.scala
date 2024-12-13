package day13

import scala.io.Source

// https://adventofcode.com/2024/day/13
object Day13:

  case class Button(x: Long, y: Long, cost: Long)
  case class Prize(x: Long, y: Long):
    def apply(delta: Long) = Prize(x + delta, y + delta)
  case class Machine(buttonA: Button, buttonB: Button, prize: Prize):
    def apply(delta: Long) = Machine(buttonA, buttonB, prize.apply(delta))
    def cost: Option[Long] = 
      val aCoeff = buttonA.x * buttonB.y - buttonA.y * buttonB.x
      val rhs = prize.x * buttonB.y - prize.y * buttonB.x
      if (rhs % aCoeff == 0)
        val a = rhs / aCoeff
        val rest = prize.x - buttonA.x * a 
        if (rest % buttonB.x == 0)
          val b = rest / buttonB.x
          Some(a * buttonA.cost + b * buttonB.cost)
        else
          None
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

  def part1(input: String): Long = 
    parse(input).map(_.cost).filter(_.isDefined).map(_.get).sum

  def part2(input: String): Long =
    parse(input).map(_.apply(10000000000000L)).map(_.cost).filter(_.isDefined).map(_.get).sum

@main def main: Unit =
  val input = Source.fromFile("input/day13.txt").getLines().mkString("\n")
  println(Day13.part1(input))
  println(Day13.part2(input))

