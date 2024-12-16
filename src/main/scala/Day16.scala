package day16

import scala.io.Source
import scala.collection.mutable.PriorityQueue
import scala.collection.mutable.HashSet

// https://adventofcode.com/2024/day/16
object Day16:

  enum Direction(val x: Int, val y: Int):
    case Up extends Direction(0, -1)
    case Down extends Direction(0, 1)
    case Left extends Direction(-1, 0)
    case Right extends Direction(1, 0)
    def turnLeft: Direction =
      this match
        case Up => Left
        case Down => Right
        case Left => Down
        case Right => Up
    def turnRight: Direction =
      this match
        case Up => Right
        case Down => Left
        case Left => Up
        case Right => Down
    
  import Direction._

  case class Position(x: Int, y: Int):
    def move(direction: Direction) = Position(x + direction.x, y + direction.y)

  enum Tile:
    case Start, End, Free, Wall
  import Tile._

  case class Node(position: Position, direction: Direction, cost: Int) extends Ordered[Node]:
    def turnLeft: Node = Node(position, direction.turnLeft, cost + 1000)
    def turnRight: Node = Node(position, direction.turnRight, cost + 1000)
    def vertices(matrix: Map[Position, Tile]): List[Node] =
      val next = position.move(direction)
      if (matrix.contains(next) && matrix(next) != Wall)
        List(Node(next, direction, cost + 1), turnLeft, turnRight)
      else
        List(turnLeft, turnRight)
    def compare(that: Node): Int = that.cost - this.cost

  def parse(input: String): Map[Position, Tile] = 
    input.linesIterator.zipWithIndex.flatMap:
      (line, y) => line.zipWithIndex.map:
        case ('S', x) => Position(x, y) -> Start
        case ('E', x) => Position(x, y) -> End
        case ('.', x) => Position(x, y) -> Free
        case ('#', x) => Position(x, y) -> Wall
    .toMap

  def shortestPath(matrix: Map[Position, Tile]): Int =
    val queue = PriorityQueue.empty[Node]
    val start = matrix.find(_._2 == Start).get._1
    queue.enqueue(Node(start, Right, 0))
    val visited = HashSet.empty[(Position, Direction)]
    var cost = Integer.MAX_VALUE
    while (!queue.isEmpty)
      val current = queue.dequeue()
      if (matrix(current.position) == End && current.cost < cost)
        cost = current.cost
      else if (visited.add((current.position, current.direction)))
        current.vertices(matrix).foreach(queue.enqueue(_))
    cost

  def part1(input: String): Int = 
    shortestPath(parse(input))

  def part2(input: String): Int = ???

@main def main: Unit =
  val input = Source.fromFile("input/day16.txt").getLines().mkString("\n")
  println(Day16.part1(input))
  println(Day16.part2(input))

