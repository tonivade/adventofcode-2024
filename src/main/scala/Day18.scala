package day18

import scala.io.Source
import scala.collection.mutable.PriorityQueue
import scala.collection.mutable.HashSet
import scala.util.boundary
import scala.collection.mutable.Queue

// https://adventofcode.com/2024/day/18
object Day18:

  case class Position(x: Int, y: Int):
    def up = Position(x, y - 1)
    def down = Position(x, y + 1)
    def left = Position(x - 1, y)
    def right = Position(x + 1, y)
    def adjacent(bounds: Bounds) = List(up, down, left, right).filter(bounds.contains)
    override def toString(): String = s"$x,$y"
 
  case class Bounds(width: Int, height: Int):
    def start = Position(0, 0)
    def stop = Position(width - 1, height - 1)
    def contains(position: Position): Boolean = 
      (0 until width).contains(position.x) && (0 until height).contains(position.y)

  case class Node(position: Position, cost: Int):
    def vertices(bounds: Bounds) = position.adjacent(bounds).map(Node(_, cost + 1))

  def parse(input: String): List[Position] =
    input.linesIterator.map(_.split(",")).map:
      case Array(x, y) => Position(x.toInt, y.toInt)
    .toList

  def search(bytes: Set[Position], bounds: Bounds): Int =
    val queue = Queue.empty[Node]
    val visited = HashSet.empty[Position]

    queue.enqueue(Node(bounds.start, 0))

    var cost = Int.MaxValue
    while (!queue.isEmpty)
      val current = queue.dequeue()
      if (current.position == bounds.stop && cost > current.cost)
        cost = current.cost
      else if (visited.add(current.position))
        current.vertices(bounds)
          .filterNot(n => bytes.contains(n.position))
          .foreach(queue.enqueue(_))
    cost

  def solve1(input: String, bounds: Bounds, number: Int): Int =
    val bytes = parse(input)
    search(bytes.take(number).toSet, bounds)

  def solve2(input: String, bounds: Bounds, number: Int): Position =
    val bytes = parse(input)
    (number until bytes.size)
      .find: x => 
        search(bytes.take(x).toSet, bounds) == Int.MaxValue
      .map(x => bytes(x - 1))
      .get

  def part1(input: String): Int = solve1(input, Bounds(71, 71), 1024)

  def part2(input: String): String = solve2(input, Bounds(71, 71), 1024).toString

@main def main: Unit =
  val input = Source.fromFile("input/day18.txt").getLines().mkString("\n")
  println(Day18.part1(input))
  println(Day18.part2(input))

