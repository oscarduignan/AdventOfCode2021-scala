//> using scala "3.1"
import scala.io.Source

// dev loop is scala-cli run . --watch
@main def adventOfCode2021() =
  println("Advent of Code 2021 in scala")
  day01_countIncreasesInDepth
  day02_whereAreWeGoing
  day03_checkDiagnostics

def day01_countIncreasesInDepth =
  def input = Source.fromFile("day01.txt").getLines.map(_.toInt)
  val part1 = input.sliding(2).count(r => r(1) > r(0))
  val part2 = input.sliding(3).map(_.sum).sliding(2).count(r => r(1) > r(0))
  println("- Day 01")
  println(s"  Part 1: $part1")
  println(s"  Part 2: $part2")

def day02_whereAreWeGoing =
  def input = Source
    .fromFile("day02.txt")
    .getLines
    .map(_.split(" "))
    .map(instruction => {
      instruction(0)(0) match {
        case 'u' => (instruction(1).toInt * -1, 0)
        case 'd' => (instruction(1).toInt, 0)
        case 'f' => (0, instruction(1).toInt)
      }
    })

  val part1 = {
    val (depth, horizontalPosition) =
      input.reduce((a, b) => (a._1 + b._1, a._2 + b._2))

    depth * horizontalPosition
  }

  val part2 = {
    val (depth, distance, _) =
      input.foldLeft((0, 0, 0))((currentPosition, nextInstruction) => {
        val (currentDepth, currentDistance, currentAim) = currentPosition
        val (aimAdjustment, movementForward) = nextInstruction
        val newAim = currentAim + aimAdjustment
        (
          currentDepth + (newAim * movementForward),
          currentDistance + movementForward,
          newAim
        )
      })

    depth * distance
  }

  println("- Day 02")
  println(s"  Part 1: $part1")
  println(s"  Part 2: $part2")

def day03_checkDiagnostics =
  val input = Source.fromFile("day03.txt").getLines.map(_.map(_.asDigit)).toList
  val halfTheNumberOfReadings = input.length / 2

  val part1_powerConsumption = {
    val readings = input.reduce((a, b) => a.zip(b).map(_ + _))
    val List(gammaRate, epsilonRate) = readings
      .map(sum => if (sum >= halfTheNumberOfReadings) then (1, 0) else (0, 1))
      .unzip
      .toList
      .map(rate => Integer.parseInt(rate.mkString, 2))
    gammaRate * epsilonRate
  }

  val part2_lifeSupport = {
    val bits = 0 until input.head.length

    val oxygenGeneratorRating = Integer.parseInt(
      bits
        .foldLeft(input)((readings, bit) =>
          readings
            .partition(reading => reading(bit) > 0)
            .toList
            .filter(_.nonEmpty)
            .sortWith(_.length > _.length)
            .head
        )
        .head
        .mkString,
      2
    )

    val CO2ScrubberRating = Integer.parseInt(
      bits
        .foldLeft(input)((readings, bit) =>
          readings
            .partition(reading => reading(bit) > 0)
            .toList
            .filter(_.nonEmpty)
            .sortWith(_.length <= _.length)
            .head
        )
        .head
        .mkString,
      2
    )

    oxygenGeneratorRating * CO2ScrubberRating
  }

  println("- Day 03")
  println(s"  Part 1: $part1_powerConsumption")
  println(s"  Part 2: $part2_lifeSupport")
