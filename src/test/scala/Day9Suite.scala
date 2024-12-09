package day9

import Day9._
import scala.collection.mutable.Buffer

class Day9Suite extends munit.FunSuite:

  val input = "2333133121414131402"

  def parse(input: String): Buffer[Sector] =
    input.map:
      case ch if ch.isDigit => Sector.File(ch.toInt - 48)
      case _  => Sector.Free
    .toBuffer
    
  test("Day9 part1 expand example 1"):
    assertEquals(expand(input), parse("00...111...2...333.44.5555.6666.777.888899"))

  test("Day9 part1 expand example 2"):
    assertEquals(expand("12345"), parse("0..111....22222"))

  test("Day9 part1 compact example 1"):
    assertEquals(compact1(parse("00...111...2...333.44.5555.6666.777.888899")), parse("0099811188827773336446555566.............."))

  test("Day9 part1 compact example 2"):
    assertEquals(compact1(parse("0..111....22222").toBuffer), parse("022111222......"))

  test("Day9 part1 checksum example 1"):
    assertEquals(checksum(parse("0099811188827773336446555566..............")), 1928L)

  test("Day9 part1"):
    assertEquals(part1(input), 1928L)

  test("Day9 part2 compact example 1".ignore):
    assertEquals(compact1(parse("00...111...2...333.44.5555.6666.777.888899")), parse("00992111777.44.333....5555.6666.....8888.."))

  test("Day9 part2 checksum example 1"):
    assertEquals(checksum(parse("00992111777.44.333....5555.6666.....8888..")), 2858L)

  test("Day9 part2".ignore):
    assertEquals(part2(input), 2858L)

