package day15

import Day15._

class Day15Suite extends munit.FunSuite:

  val input = """##########
                |#..O..O.O#
                |#......O.#
                |#.OO..O.O#
                |#..O@..O.#
                |#O#..O...#
                |#O..O..O.#
                |#.OO.O.OO#
                |#....O...#
                |##########
                |
                |<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
                |vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
                |><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
                |<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
                |^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
                |^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
                |>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
                |<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
                |^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
                |v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^""".stripMargin

  test("Day15 part1 example 1"):
    assertEquals(part1("""########
                         |#..O.O.#
                         |##@.O..#
                         |#...O..#
                         |#.#.O..#
                         |#...O..#
                         |#......#
                         |########
                         |
                         |<^^>>>vv<v>>v<<""".stripMargin), 2028)

  test("Day15 part1"):
    assertEquals(part1(input), 10092)

  test("Day15 part2".ignore):
    assertEquals(part2(input), 1)

