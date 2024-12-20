package day19

import scala.io.Source
import scala.collection.mutable.HashMap

// https://adventofcode.com/2024/day/19
object Day19:

  def search(pattern: String, towels: List[String], cache: HashMap[String, Boolean]): Boolean = 
  
    def memoized(chunk: String): List[String] =
      if cache.contains(chunk) then
        if cache(chunk) then
          pattern :: Nil
        else 
          Nil
      else
        val result = loop(chunk)
        cache.put(chunk, !result.isEmpty)
        result

    def loop(chunk: String): List[String] =
      val index = towels.indexWhere(_ == chunk)
      if (index > -1)
        List(pattern)
      else
        towels
          .filter(_.size < chunk.size)
          .filter(chunk.startsWith(_))
          .flatMap(partial => memoized(chunk.drop(partial.size)))

    !memoized(pattern).isEmpty

  def part1(input: String): Int = 
    val (towels, patterns) = input.split("\n\n") match
      case Array(top, bottom) => 
        (top.split(",").map(_.trim).toList, bottom.linesIterator.toList)

    val cache = HashMap.empty[String, Boolean]
    patterns.filter(search(_, towels, cache)).size
    
  def part2(input: String): Int = ???

@main def main: Unit =
  val input = Source.fromFile("input/day19.txt").getLines().mkString("\n")
  println(Day19.part1(input))
  println(Day19.part2(input))

