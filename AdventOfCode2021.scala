//> using scala "3.1"
import scala.io.Source

// dev loop is scala-cli run . --watch
@main def adventOfCode2021() =
  println("Advent of Code 2021 in scala")
  day01_countIncreasesInDepth
  day02_whereAreWeGoing
  day03_checkDiagnostics
  day04_squidBingo

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

def day04_squidBingo =
  val input = Source.fromFile("day04.txt").getLines
  val draws = input.next().split(",").toList.map(_.toInt)
  val boards = input.filter(_.nonEmpty).map(_.split("\\s+").filter(_.nonEmpty).toList.map(_.toInt)).grouped(5).map(board => board ++ board.transpose).toList

  // These would probably read so much more clearly if I extracted some methods for updating
  // the boards and matching winning boards. I don't like seeing the map -> map -> map - I 
  // find that quite hard to mentally parse.

  /**
   * I got stuck on this for a while until I used recursion and then it clicked, I think I
   * was also getting slowed trying to over optimize it loads, and it went better when I 
   * let go of that self imposed constraint - for example just mapping through numbers in
   * a line when I know there will be only one that might be updated, and not doing anything
   * fancy when I find it to linear-algebra the index for the number in the line along the
   * other axis (since my boards contain all vertical and horziontal lines to make it easy)
   */
  @scala.annotation.tailrec
  def findWinningScore(remainingDraws: List[Int], boards: List[Seq[Seq[Int]]]): Option[Int] = remainingDraws match {
     case Nil => None // no winners!
     case numberDrawn :: stillRemainingDraws =>
       val updatedBoards = boards.map(board => board.map(line => line.map(number =>
         if number == numberDrawn
         then number * -1
         else number
       )))
  
       updatedBoards.find(board => board.exists(line => line.forall(_ < 0))) match {
         case Some(winningBoard) => Some(calculateScore(numberDrawn, winningBoard))
         case _                  => findWinningScore(stillRemainingDraws, updatedBoards)
       }
  }

  /**
   * This one was interesting, realised I could just replace marked numbers with a sentinel
   * value of -1, because otherwise I would mess up if the number was 0. I also got caught
   * out by "filter" is not "exclude" when calculating remaining boards without winners. And
   * before I discovered that I had thought maybe I wasn't handling multiple winners in one
   * go so I refactored to allow for that as well.
   */
  @scala.annotation.tailrec
  def findLastWinningScore(remainingDraws: List[Int], boards: List[Seq[Seq[Int]]], winners: List[(Int, Seq[Seq[Int]])]): Option[Int] = remainingDraws match {
     case Nil => winners match {
       case Nil => None
       case (winningNumber, winningBoard) :: earlierWinners => 
         Some(calculateScore(winningNumber, winningBoard))
     }
     case numberDrawn :: stillRemainingDraws =>
       val updatedBoards = boards.map(board => board.map(line => line.map(number =>
         if number == numberDrawn
         then -1
         else number
       ))).zipWithIndex
  
       updatedBoards.filter(board => board._1.exists(line => line.forall(_ == -1))) match {
         case Nil             => findLastWinningScore(stillRemainingDraws, updatedBoards.map(_._1), winners)
         case winnersThisTurn =>
           val indexesOfWinnersThisTurn = winnersThisTurn.map(_._2)
           val boardsWithoutWinners = updatedBoards.filterNot(b => indexesOfWinnersThisTurn.contains(b._2)).map(_._1)
           val allWinnersSoFar = winnersThisTurn.map(numberDrawn -> _._1) ++ winners

           findLastWinningScore(stillRemainingDraws, boardsWithoutWinners, allWinnersSoFar)
       }
  }
  
  def calculateScore(winningNumber: Int, winningBoard: Seq[Seq[Int]]) =
     winningBoard.take(winningBoard.length/2).flatten.filter(_ > 0).sum * winningNumber

  println("- Day 04")
  println(s"  Part 1: ${findWinningScore(draws, boards)}")
  println(s"  Part 2: ${findLastWinningScore(draws, boards, Nil)}")
