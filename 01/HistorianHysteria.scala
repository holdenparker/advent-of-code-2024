
@main
def HistorianHysteria(): Unit = {
  // val fileName = "testData.txt"
  val fileName = "data.txt"
  val (listA, listB) = _parseData(fileName)
  PartOne(listA, listB);
}

def _parseData(fileName: String): (Array[Int], Array[Int]) = {
  val file = io.Source.fromFile(fileName)

  var listA = Array[Int]()
  var listB = Array[Int]()

  for (line <- file.getLines()) {
    val items = line.split("\\s+")
    listA = listA ++ Array(items(0).toInt)
    listB = listB ++ Array(items(1).toInt)
  }

  return (listA, listB)
}

def PartOne(listA: Array[Int], listB: Array[Int]): Unit = {
  val sortedA = listA.sortWith(_ < _)
  val sortedB = listB.sortWith(_ < _)

  var result = Array[Int]()
  for (i <- 0 to sortedA.length - 1) {
    result = result ++ Array((sortedA(i) - sortedB(i)).abs)
  }

  printf("Result of part one: %d\n", result.sum)
}