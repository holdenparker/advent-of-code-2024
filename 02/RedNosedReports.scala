import scala.util.boundary, boundary.break
import scala.compiletime.ops.double

@main
def RedNosedReports(): Unit = {
  // val fileName = "testData.txt"
  val fileName = "data.txt"

  val part1 = PartOneReportAnalyzer()
  _parseData(fileName, part1)
  printf("There are %d safe reports in part 1!\n", part1.safeReports)

}

trait RedNoseReactorReportAnalyzer:
  def processLine(line: Array[Int]): Unit

class PartOneReportAnalyzer extends RedNoseReactorReportAnalyzer {
  var safeReports = 0

  override def processLine(line: Array[Int]): Unit = {
    val goingUp = if (line(0) > line(1)) false else true

    boundary:
      for i <- 1 to line.length - 1 do
        val diff = line(i - 1) - line(i)
        if (diff.abs < 1 || diff.abs > 3) then break()
        if ((diff < 0) != goingUp) then break()
      safeReports += 1
  }
}

def _parseData(fileName: String, analyzer: RedNoseReactorReportAnalyzer): Unit = {
  val file = io.Source.fromFile(fileName)

  for line <- file.getLines() do
    analyzer.processLine(line.split("\\s+").map(_.toInt))
}