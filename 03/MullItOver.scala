import scala.util.matching.Regex
import scala.compiletime.ops.double

@main
def MullItOver(): Unit = {
  // val fileName = "testData.txt"
  // val fileName = "testData2.txt"
  val fileName = "data.txt"

  val part1 = PartOneParser()
  _parseData(fileName, part1)
  printf("The product summation is %d in part 1!\n", part1.total)

  val part2 = PartTwoParser()
  _parseData(fileName, part2)
  printf("The product summation is %d in part 2!\n", part2.total)
}

trait MullParser:
  def processLine(line: String): Unit

class PartOneParser extends MullParser {
  var total = 0

  override def processLine(line: String): Unit = {
    val mulMatcher: Regex = "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)".r

    for m <- mulMatcher.findAllMatchIn(line) do
      total += m.group(1).toInt * m.group(2).toInt
  }
}

class PartTwoParser extends MullParser {
  var total = 0
  var enabled = true

  override def processLine(line: String): Unit = {
    val mulMatcher: Regex = "do\\(\\)|don't\\(\\)|mul\\(([0-9]{1,3}),([0-9]{1,3})\\)".r

    for m <- mulMatcher.findAllMatchIn(line) do
      if (m.group(0) == "do()") {
        enabled = true
      } else if (m.group(0) == "don't()") {
        enabled = false
      } else if (enabled == true) {
        total += m.group(1).toInt * m.group(2).toInt
      }
  }
}

def _parseData(fileName: String, parser: MullParser): Unit = {
  val file = io.Source.fromFile(fileName)

  for line <- file.getLines() do
    parser.processLine(line)
}