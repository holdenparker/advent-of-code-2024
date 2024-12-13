import scala.util.boundary, boundary.break
import scala.compiletime.ops.double

@main
def RedNosedReports(): Unit = {
  // val fileName = "testData.txt"
  val fileName = "data.txt"

  val part1 = PartOneReportAnalyzer()
  _parseData(fileName, part1)
  printf("There are %d safe reports in part 1!\n", part1.safeReports)

  val part2 = PartTwoReportAnalyzer()
  _parseData(fileName, part2)
  printf("There are %d safe reports in part 2!\n", part2.safeReports)
}

trait RedNoseReactorReportAnalyzer:
  def processLine(line: Array[Int]): Unit

abstract class BaseReportAnalyzer {

  def goingUp(line: Array[Int]) = line(0) < line(1)

  def changeIsSafe(a: Int, b: Int, goingUp: Boolean): Boolean = {
    val diff = a - b
    if (diff.abs < 1 || diff.abs > 3) then return false
    if ((diff < 0) != goingUp) then return false
    return true
  }
}

class PartOneReportAnalyzer extends BaseReportAnalyzer, RedNoseReactorReportAnalyzer {
  var safeReports = 0

  override def processLine(line: Array[Int]): Unit = {
    val _goingUp = goingUp(line)

    boundary:
      for i <- 1 to line.length - 1 do
        if (!changeIsSafe(line(i - 1), line(i), _goingUp)) then break()
      safeReports += 1
  }
}

class PartTwoReportAnalyzer extends BaseReportAnalyzer, RedNoseReactorReportAnalyzer {
  var safeReports = 0

  override def processLine(line: Array[Int]): Unit = {
    val _goingUp = goingUp(line)

    var isSafe = checkLineIsSafe(line)
    boundary:
      for i <- 0 to line.length - 1 do
        if (isSafe) then break()
        isSafe = checkLineIsSafe(line.slice(0, i) ++ line.slice(i + 1, line.length))
    if (isSafe) then safeReports += 1
  }

  def checkLineIsSafe(line: Array[Int]): Boolean = {
    val _goingUp = goingUp(line)
    var result = true

    boundary:
      for i <- 1 to line.length - 1 do
        if (!changeIsSafe(line(i - 1), line(i), _goingUp)) {
          result = false
          break()
        }
    return result;
  }
}

def _parseData(fileName: String, analyzer: RedNoseReactorReportAnalyzer): Unit = {
  val file = io.Source.fromFile(fileName)

  for line <- file.getLines() do
    analyzer.processLine(line.split("\\s+").map(_.toInt))
}