package day17

import scala.io.Source
import scala.annotation.tailrec

// https://adventofcode.com/2024/day/17
object Day17:

  case class Computer(a: Int, b: Int, c: Int, pointer: Int = 0, output: List[Int] = List.empty):

    def combo(operand: Int): Int =
      operand match
        case 4 => a
        case 5 => b
        case 6 => c
        case _ => operand
 
    def step(opcode: Int, operand: Int): Computer =
      opcode match
        case 0 => Computer(a / math.pow(2, combo(operand)).toInt, b, c, pointer + 2, output)
        case 1 => Computer(a, b ^ operand, c, pointer + 2, output)
        case 2 => Computer(a, combo(operand) % 8, c, pointer + 2, output)
        case 3 if a == 0 => Computer(a, b, c, pointer + 2, output)
        case 3 => Computer(a, b, c, operand, output)
        case 4 => Computer(a, b ^ c, c, pointer + 2, output)
        case 5 => Computer(a, b, c, pointer + 2, output :+ combo(operand) % 8)
        case 6 => Computer(a, a / math.pow(2, combo(operand)).toInt, c, pointer + 2, output)
        case 7 => Computer(a, b, a / math.pow(2, combo(operand)).toInt, pointer + 2, output)
    
  @tailrec
  def exec(computer: Computer)(program: Vector[Int]): Computer =
    println(s"$computer")
    if (computer.pointer < program.size)
      val opcode = program(computer.pointer)
      val operand = program(computer.pointer + 1)
      exec(computer.step(opcode, operand))(program)
    else
      computer

  def parseComputer(input: String): Computer = 
    input.split("\n") match
      case Array(a, b, c) => Computer(
        a.split(":")(1).trim().toInt,
        b.split(":")(1).trim().toInt,
        c.split(":")(1).trim().toInt
      )

  def parseProgram(input: String): Vector[Int] = 
    input.split(":")(1).trim().split(",").map(_.toInt).toVector

  def parse(input: String): (Computer, Vector[Int]) = 
    input.split("\n\n") match
      case Array(top, bottom) => (parseComputer(top), parseProgram(bottom))

  def part1(input: String): String = 
    val (computer, program) = parse(input)
    exec(computer)(program).output.mkString(",")
  def part2(input: String): Int = ???

@main def main: Unit =
  val input = Source.fromFile("input/day17.txt").getLines().mkString("\n")
  println(Day17.part1(input))
  println(Day17.part2(input))

