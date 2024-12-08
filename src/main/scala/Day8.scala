package day8

import scala.io.Source

// https://adventofcode.com/2024/day/8
object Day8:

  case class Bounds(x: Int, y: Int):
    def contains(p: Position): Boolean = 
      p.x >= 0 && p.x <= x && p.y >= 0 && p.y <= y

  object Bounds:
    def apply(matrix: Set[Position]): Bounds = 
      Bounds(matrix.map(_.x).max, matrix.map(_.y).max)
  
  case class Position(x: Int, y: Int):

    def antinodes1(other: Position, amplify: Int = 1): List[Position] = 
      val diffx = (this.x - other.x) * amplify
      val diffy = (this.y - other.y) * amplify

      List(
        Position(this.x + diffx, this.y + diffy),
        Position(other.x - diffx, other.y - diffy),
      )

    def antinodes2(other: Position, bounds: Bounds): List[Position] = 
      LazyList.from(1)
        .map(this.antinodes1(other, _))
        .takeWhile(_.exists(bounds.contains))
        .flatMap(identity)
        .filter(bounds.contains)
        .toList

  def parse(input: String): Map[Position, Char] =
    input.linesIterator.zipWithIndex.flatMap:
      (line, y) => line.zipWithIndex.map:
        (ch, x) => Position(x, y) -> ch
    .toMap
  
  def findAntennas(matrix: Map[Position, Char]): Set[List[Position]] =
    matrix.values.toSet.filterNot(_ == '.').map:
      a => matrix.filter((_, ch) => ch == a).keySet.toList

  def findAntinodes1(antennas: Set[List[Position]]): Set[Position] =
    antennas.flatMap:
      _.combinations(2).toList.flatMap:
        case List(a, b) => a.antinodes1(b)
    .toSet

  def findAntinodes2(antennas: Set[List[Position]], bounds: Bounds): Set[Position] =
    antennas.flatMap:
      _.combinations(2).toList.flatMap:
        case List(a, b) => a.antinodes2(b, bounds)
    .toSet

  def part1(input: String): Int = 
    val matrix = parse(input)
    val antennas = findAntennas(matrix)
    findAntinodes1(antennas).filter(matrix.contains).size

  def part2(input: String): Int =
    val matrix = parse(input)
    val antennas = findAntennas(matrix)
    val antinodes = findAntinodes2(antennas, Bounds(matrix.keySet)).toSet
    (antinodes ++ antennas.flatMap(identity)).size

@main def main: Unit =
  val input = Source.fromFile("input/day8.txt").getLines().mkString("\n")
  println(Day8.part1(input))
  println(Day8.part2(input))

