package day5

import scala.io.Source

// https://adventofcode.com/2024/day/5
object Day5:

  case class Rule(a: Int, b: Int):
    def check(update: List[Int]): Boolean = 
      if (update.contains(a) && update.contains(b))
        update.indexOf(a) < update.indexOf(b)
      else
        true

    def eq(x: Int, y: Int): Boolean = a == x && b == y

  def parseRules(input: String): List[Rule] =
    input.linesIterator.map(_.split("\\|")).map:
      case Array(a, b) => Rule(a.toInt, b.toInt)
    .toList

  def parseUpdates(input: String): List[List[Int]] =
    input.linesIterator.map(_.split(",").map(_.toInt).toList).toList

  def parse(input: String): (List[Rule], List[List[Int]]) =
    input.split("\n\n") match
      case Array(part1, part2) => (parseRules(part1), parseUpdates(part2))

  def part1(input: String): Int =
    val (rules, updates) = parse(input)

    updates.filter(update => rules.forall(_.check(update)))
      .map(update => update(update.size / 2))
      .sum

  def part2(input: String): Int = 
    val (rules, updates) = parse(input)
    
    updates.filterNot(update => rules.forall(_.check(update)))
      .map(_.sortWith((a, b) => rules.exists(_.eq(a, b))))
      .map(update => update(update.size / 2))
      .sum

@main def main: Unit =
  val input = Source.fromFile("input/day5.txt").getLines().mkString("\n")
  println(Day5.part1(input))
  println(Day5.part2(input))

