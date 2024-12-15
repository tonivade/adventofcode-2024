package day15

import scala.io.Source
import scala.annotation.tailrec

// https://adventofcode.com/2024/day/15
object Day15:

  case class Position(x: Int, y: Int):
    def move(m: Move) = Position(x + m.x, y + m.y)
    def gps: Int = 100 * y + x

  enum Tile:
    case Robot, Box, Wall, Free

  import Tile._

  enum Move(val x: Int, val y: Int):
    case Up extends Move(0, -1)
    case Down extends Move(0, 1)
    case Left extends Move(-1, 0)
    case Right extends Move(1, 0)

  import Move._

  def parseMap(input: String): Map[Position, Tile] = 
    input.linesIterator.zipWithIndex.flatMap:
      case (line, y) => line.zipWithIndex.map:
        case ('#', x) => Position(x, y) -> Wall
        case ('@', x) => Position(x, y) -> Robot
        case ('O', x) => Position(x, y) -> Box
        case ('.', x) => Position(x, y) -> Free
    .toMap

  def parseMoves(input: String): List[Move] = 
    input.linesIterator.flatMap:
      case line => line.map:
        case '^' => Up
        case 'v' => Down
        case '>' => Right
        case '<' => Left
    .toList

  def step(matrix: Map[Position, Tile], current: Position, move: Move): (Map[Position, Tile], Position) = 
    val next = current.move(move)
    matrix(next) match
      case Box => 
        val (nextMatrix, _) = step(matrix, next, move)
        if (nextMatrix(next) == Free)
          (nextMatrix + (next -> matrix(current)) + (current -> Free), next)
        else
          (matrix, current)
      case Free => (matrix + (next -> matrix(current)) + (current -> Free), next)
      case _ => (matrix, current)
  
  @tailrec
  def walk(matrix: Map[Position, Tile], moves: List[Move], robot: Position): (Map[Position, Tile], Position) =
    moves match
      case head :: tail => 
        val (nextMatrix, nextRobot) = step(matrix, robot, head)
        walk(nextMatrix, tail, nextRobot)
      case Nil => (matrix, robot)

  def mkString(matrix: Map[Position, Tile]): String = 
    val maxX = matrix.keySet.map(_._1).max + 1
    val maxY = matrix.keySet.map(_._2).max + 1
    val buffer = new StringBuffer
    buffer.append(" ")
    for (x <- 0 until maxX)
      buffer.append(x % 10)
    buffer.append("\n")
    for (y <- 0 until maxY)
      buffer.append(y % 10)
      for (x <- 0 until maxX)
        val tile = matrix(Position(x, y)) match
          case Free => "."
          case Wall => "#"
          case Box => "O"
          case Robot => "@"
        buffer.append(tile)
      buffer.append("\n")
    buffer.toString

  def part1(input: String): Int = 
    val (matrix, moves) = input.split("\n\n") match
      case Array(top, bottom) =>
        (parseMap(top), parseMoves(bottom))

    val robot = matrix.find(_._2 == Robot).get._1
    val (end, _) = walk(matrix, moves, robot)
    println(mkString(end))
    end.filter(_._2 == Box).keySet.map(_.gps).sum

  def part2(input: String): Int = ???

@main def main: Unit =
  val input = Source.fromFile("input/day15.txt").getLines().mkString("\n")
  println(Day15.part1(input))
  println(Day15.part2(input))

