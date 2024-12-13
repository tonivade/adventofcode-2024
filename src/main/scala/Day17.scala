package day17

import scala.io.Source
import scala.annotation.tailrec
import scala.collection.mutable.HashSet

// https://adventofcode.com/2024/day/17
object Day17:

  case class Computer(a: Long, b: Long, c: Long, pointer: Int = 0, output: List[Int] = List.empty):

    def combo(operand: Int): Long =
      operand match
        case 4 => a
        case 5 => b
        case 6 => c
        case _ => operand
    
    def dv(x: Long, operand: Int): Long = x / math.pow(2, combo(operand)).toInt

    def mod8(operand: Int): Int = (combo(operand) % 8).toInt
 
    def step(opcode: Int, operand: Int): Computer =
      opcode match
        case 0 => Computer(dv(a, operand), b, c, pointer + 2, output)
        case 1 => Computer(a, b ^ operand, c, pointer + 2, output)
        case 2 => Computer(a, mod8(operand), c, pointer + 2, output)
        case 3 if a == 0 => Computer(a, b, c, pointer + 2, output)
        case 3 => Computer(a, b, c, operand, output)
        case 4 => Computer(a, b ^ c, c, pointer + 2, output)
        case 5 => Computer(a, b, c, pointer + 2, output :+ mod8(operand))
        case 6 => Computer(a, dv(a, operand), c, pointer + 2, output)
        case 7 => Computer(a, b, dv(a, operand), pointer + 2, output)
    
  @tailrec
  def exec(program: Vector[Int])(computer: Computer): Computer =
    if (computer.pointer < program.size)
      val opcode = program(computer.pointer)
      val operand = program(computer.pointer + 1)
      exec(program)(computer.step(opcode, operand))
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
    exec(program)(computer).output.mkString(",")

  def toDecimal(input: Iterable[Int]): Long =
    input.zipWithIndex.map:
      case (x, i) => x.toLong * math.pow(8, i).toLong
    .sum

  def solve2(program: Vector[Int]): Long = 
    val validValuesOfA = HashSet(0L)
    program.reverseIterator.foreach: digit =>
      val nextValuesOfA = HashSet.empty[Long]
      validValuesOfA.foreach: a =>
        val shifted = a * 8L

        (shifted to shifted + 8).foreach: candidate =>
          val output = exec(program)(Computer(candidate, 0, 0)).output
          if (output(0) == digit)
            println(s"$candidate: $output")
            nextValuesOfA.add(candidate)

      validValuesOfA.clear()
      validValuesOfA.addAll(nextValuesOfA)

    validValuesOfA.filter: a => 
      program.startsWith(exec(program)(Computer(a, 0, 0)).output)
    .min

  def part2(input: String): Long = 
    val (_, program) = parse(input)
    val target = toDecimal(program)
    val result = solve2(program)

    val output = exec(program)(Computer(result, 0, 0)).output
    println(program.mkString)
    println(output.mkString)

    result

@main def main: Unit =
  val input = Source.fromFile("input/day17.txt").getLines().mkString("\n")
  println(Day17.part1(input))
  println(Day17.part2(input))

