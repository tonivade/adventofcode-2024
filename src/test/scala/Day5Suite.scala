package day5

import Day5._

class Day5Suite extends munit.FunSuite:

  val input = """47|53
                |97|13
                |97|61
                |97|47
                |75|29
                |61|13
                |75|53
                |29|13
                |97|29
                |53|29
                |61|53
                |97|53
                |61|29
                |47|13
                |75|47
                |97|75
                |47|61
                |75|61
                |47|29
                |75|13
                |53|13
                |
                |75,47,61,53,29
                |97,61,53,29,13
                |75,29,13
                |75,97,47,61,53
                |61,13,29
                |97,13,75,29,47""".stripMargin

  test("check rule") {
    val update = List(75, 47, 61, 53, 29)
    val rule = Rule(97, 28)

    assert(rule.check(update))
  }

  test("check rule not contain") {
    val update = List(75, 47, 61, 53, 29)
    val rule = Rule(47, 53)

    assert(rule.check(update))
  }

  test("not check rule not contain") {
    val update = List(75, 47, 61, 53, 29)
    val rule = Rule(53, 47)

    assert(!rule.check(update))
  }

  test("Day5 part1") {
    assertEquals(part1(input), 143)
  }

  test("Day5 part2") {
    assertEquals(part2(input), 123)
  }

