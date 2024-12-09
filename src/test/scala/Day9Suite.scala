package day9

import Day9._

class Day9Suite extends munit.FunSuite:

  val input = "2333133121414131402"

  def parse(input: String): List[Sector] =
    input.map:
      case ch if ch.isDigit => Sector.File(ch.toInt - 48)
      case _  => Sector.Free
    .toList
    

  test("Day9 part1 expand example 1"):
    assertEquals(expand(input), parse("00...111...2...333.44.5555.6666.777.888899"))

  test("Day9 part1 expand example 2"):
    assertEquals(expand("12345"), parse("0..111....22222"))

  test("Day9 part1 compact example 1"):
    assertEquals(compact(parse("00...111...2...333.44.5555.6666.777.888899")), parse("0099811188827773336446555566.............."))

  test("Day9 part1 compact example 2"):
    assertEquals(compact(parse("0..111....22222")), parse("022111222......"))

  test("Day9 part1 checksum example 1"):
    assertEquals(checksum(parse("0099811188827773336446555566..............")), 1928L)

  test("Day9 part1"):
    assertEquals(part1(input), 1928L)

  test("Day9 part2"):
    assertEquals(part2(input), 1)

