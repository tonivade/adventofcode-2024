package day16

import scala.io.Source
import scala.annotation.tailrec

// https://adventofcode.com/2024/day/16
object Day16:

  enum Direction:
    case Up, Down, Left, Right
    def turn(other: Direction): Boolean =
      (this, other) match
        case (Up, Up) => false
        case (Down, Down) => false
        case (Left, Left) => false
        case (Right, Right) => false
        case (Up, Left) => true
        case (Up, Right) => true
        case (Down, Left) => true
        case (Down, Right) => true
        case (Left, Up) => true
        case (Left, Down) => true
        case (Right, Up) => true
        case (Right, Down) => true
        case _ => throw RuntimeException("not possible")
      
  import Direction._

  case class Position(x: Int, y: Int):
    def up = Position(x, y - 1)
    def down = Position(x, y + 1)
    def left = Position(x - 1, y)
    def right = Position(x + 1, y)
    def adjacent = List(up, down, left, right)

  enum Tile:
    case Start, End, Free, Wall
  import Tile._

  enum Tree:
    case Node(position: Position, up: Tree, down: Tree, left: Tree, right: Tree)
    case Target(cost: Int)
    case Dead
    def leaves: List[Target] =
      this match
        case t:Target => List(t)
        case Dead => List()
        case Node(_, up, down, left, right) => up.leaves ++ down.leaves ++ left.leaves ++ right.leaves
  import Tree._

  def parse(input: String): Map[Position, Tile] = 
    input.linesIterator.zipWithIndex.flatMap:
      (line, y) => line.zipWithIndex.map:
        case ('S', x) => Position(x, y) -> Start
        case ('E', x) => Position(x, y) -> End
        case ('.', x) => Position(x, y) -> Free
        case ('#', x) => Position(x, y) -> Wall
    .toMap

  def search(matrix: Map[Position, Tile], current: Position, direction: Direction = Right, cost: Int = 0, visited: Set[Position] = Set.empty): Tree =
    if (matrix(current) == End)
      Target(cost)
    else
      val up: Tree =
        if (!matrix.contains(current.up) || visited.contains(current.up) || matrix(current.up) == Wall)
          Dead
        else 
          search(matrix, current.up, Up, cost + (if(direction.turn(Up)) then 1001 else 1), visited + current)

      val down: Tree =
        if (!matrix.contains(current.down) || visited.contains(current.down) || matrix(current.down) == Wall)
          Dead
        else 
          search(matrix, current.down, Down, cost + (if(direction.turn(Down)) then 1001 else 1), visited + current)

      val left: Tree =
        if (!matrix.contains(current.left) || visited.contains(current.left) || matrix(current.left) == Wall)
          Dead
        else 
          search(matrix, current.left, Left, cost + (if(direction.turn(Left)) then 1001 else 1), visited + current)

      val right: Tree =
        if (!matrix.contains(current.right) || visited.contains(current.right) || matrix(current.right) == Wall)
          Dead
        else 
          search(matrix, current.right, Right, cost + (if(direction.turn(Right)) then 1001 else 1), visited + current)

      Node(current, up, down, left, right)

  def part1(input: String): Int = 
    val matrix = parse(input)
    val start = matrix.find(_._2 == Start).get._1
    val tree = search(matrix, start)
    tree.leaves.map(_.cost).min

  def part2(input: String): Int = ???

@main def main: Unit =
  val input = Source.fromFile("input/day16.txt").getLines().mkString("\n")
  println(Day16.part1(input))
  println(Day16.part2(input))

