package day19

import scala.io.Source
import scala.collection.mutable.HashMap

// https://adventofcode.com/2024/day/19
object Day19:

  def search(pattern: String, towels: List[String], cache: HashMap[String, Long]): Long = 

    def memoized(chunk: String): Long =
      if (cache.contains(chunk))
        cache(chunk)
      else
        val result = loop(chunk)
        cache.put(chunk, result)
        result
  
    def loop(chunk: String): Long =
      towels
        .filter(chunk.startsWith)
        .map: partial => 
          if (partial == chunk)
            1
          else 
            memoized(chunk.drop(partial.size))
        .sum
    
    memoized(pattern)

  def parse(input: String): (List[String], List[String]) =
    input.split("\n\n") match
      case Array(top, bottom) => 
        (top.split(",").map(_.trim).toList, bottom.linesIterator.toList)

  def part1(input: String): Int = 
    val (towels, patterns) = parse(input)
    val cache = HashMap.empty[String, Long]
    patterns.map(search(_, towels, cache)).count(_ > 0)
    
  def part2(input: String): Long = 
    val (towels, patterns) = parse(input)
    val cache = HashMap.empty[String, Long]
    patterns.map(search(_, towels, cache)).sum

@main def main: Unit =
  val input = Source.fromFile("input/day19.txt").getLines().mkString("\n")
  println(Day19.part1(input))
  println(Day19.part2(input))

