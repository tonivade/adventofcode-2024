package day4

import scala.io.Source

// https://adventofcode.com/2024/day/4
object Day4:

  case class Position(x: Int, y: Int):
    def right = Position(x + 1, y)
    def left = Position(x - 1, y)
    def up = Position(x, y + 1)
    def down = Position(x, y - 1)
    def rightUp = Position(x + 1, y + 1)
    def rightDown = Position(x + 1, y - 1)
    def leftUp = Position(x - 1, y + 1)
    def leftDown = Position(x - 1, y - 1)

  def parse(input: String): Map[Position, Char] =
    input.linesIterator.zipWithIndex.flatMap:
      (line, y) => line.zipWithIndex.map:
        (ch, x) => Position(x, y) -> ch
    .toMap

  def part1(input: String): Int = 
    val matrix = parse(input)
    
    val xs = matrix.filter:
      case (_, ch) => ch == 'X'
    .keySet
    val ms = matrix.filter:
      case (_, ch) => ch == 'M'
    .keySet
    val as = matrix.filter:
      case (_, ch) => ch == 'A'
    .keySet
    val ss = matrix.filter:
      case (_, ch) => ch == 'S'
    .keySet
    
    val ups = xs.filter:
      x => ms.contains(x.up) && as.contains(x.up.up) && ss.contains(x.up.up.up)
    .size
    val downs = xs.filter:
      x => ms.contains(x.down) && as.contains(x.down.down) && ss.contains(x.down.down.down)
    .size
    val lefts = xs.filter:
      x => ms.contains(x.left) && as.contains(x.left.left) && ss.contains(x.left.left.left)
    .size
    val rights = xs.filter:
      x => ms.contains(x.right) && as.contains(x.right.right) && ss.contains(x.right.right.right)
    .size
    val rightUps = xs.filter:
      x => ms.contains(x.rightUp) && as.contains(x.rightUp.rightUp) && ss.contains(x.rightUp.rightUp.rightUp)
    .size
    val rightDowns = xs.filter:
      x => ms.contains(x.rightDown) && as.contains(x.rightDown.rightDown) && ss.contains(x.rightDown.rightDown.rightDown)
    .size
    val leftUps = xs.filter:
      x => ms.contains(x.leftUp) && as.contains(x.leftUp.leftUp) && ss.contains(x.leftUp.leftUp.leftUp)
    .size
    val leftDowns = xs.filter:
      x => ms.contains(x.leftDown) && as.contains(x.leftDown.leftDown) && ss.contains(x.leftDown.leftDown.leftDown)
    .size

    lefts + rights + ups + downs + rightUps + rightDowns + leftUps + leftDowns

  def part2(input: String): Int = 
    val matrix = parse(input)
    
    val as = matrix.filter:
      case (_, ch) => ch == 'A'
    .keySet
    val ms = matrix.filter:
      case (_, ch) => ch == 'M'
    .keySet
    val ss = matrix.filter:
      case (_, ch) => ch == 'S'
    .keySet
    
    /* 
        M.S
        .A.
        M.S
     */
    val r1 = as.filter:
      a => ms.contains(a.leftUp) && ms.contains(a.leftDown) && ss.contains(a.rightUp) && ss.contains(a.rightDown)
    .size

    /* 
        S.M
        .A.
        S.M
     */
    val r2 = as.filter:
      a => ss.contains(a.leftUp) && ss.contains(a.leftDown) && ms.contains(a.rightUp) && ms.contains(a.rightDown)
    .size

    /* 
        M.M
        .A.
        S.S
     */
    val r3 = as.filter:
      a => ms.contains(a.leftUp) && ms.contains(a.rightUp) && ss.contains(a.leftDown) && ss.contains(a.rightDown)
    .size

    /* 
        S.S
        .A.
        M.M
     */
    val r4 = as.filter:
      a => ss.contains(a.leftUp) && ss.contains(a.rightUp) && ms.contains(a.leftDown) && ms.contains(a.rightDown)
    .size

    r1 + r2 + r3 + r4

@main def main: Unit =
  val input = Source.fromFile("input/day4.txt").getLines().mkString("\n")
  println(Day4.part1(input))
  println(Day4.part2(input))

