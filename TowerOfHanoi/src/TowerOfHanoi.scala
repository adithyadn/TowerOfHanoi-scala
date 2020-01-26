import scala.collection.immutable.TreeMap
import scala.annotation.tailrec

def printPegs(listOfStepResults: List[TreeMap[Int, List[Int]]])
{
  listOfStepResults.foreach{ listOfPegs =>
    listOfPegs.foreach{ pegs =>
      print(pegs._2.mkString)
      print("\t")
    }
    println()
  }
}

def getInitialPegMap(numberOfDisks: Int) = {
  val startPeg = (1 to numberOfDisks).toList
  val sparePeg = List[Int]()
  val destinationPeg = List[Int]()
  List(TreeMap(0 -> startPeg, 1 -> sparePeg, 2 -> destinationPeg))
}

def movePegs(listOfPegs: List[TreeMap[Int, List[Int]]], sourceIndex: Int, destinationIndex: Int) = {

  val temporaryIndex = 3 - (sourceIndex + destinationIndex)

  TreeMap(sourceIndex -> listOfPegs(0).getOrElse(sourceIndex, List()).tail,
    temporaryIndex -> listOfPegs(0).getOrElse(temporaryIndex, List()),
    destinationIndex -> (listOfPegs(0).getOrElse(sourceIndex, List()).head :: listOfPegs(0).getOrElse(destinationIndex, List())
      )) :: listOfPegs
}


def startIterativeTowerOfHanoi(numberOfDisks: Int) =
{
  def towerOfHanoiIterative(noOfDisks: Int) = {
    (1 to (1 << noOfDisks) - 1).foldLeft(getInitialPegMap(noOfDisks)) { (listOfPegs, stepNumber) =>
      movePegs(listOfPegs, (stepNumber & stepNumber - 1) % 3, ((stepNumber | stepNumber - 1) + 1) % 3)
    }
  }
  towerOfHanoiIterative(numberOfDisks).reverse
}

def startPureRecursive(numberOfDisks: Int) =
{
  def pureRecursive(listOfPegs: List[TreeMap[Int, List[Int]]], disk: Int, sourceIndex: Int,
                    destinationIndex: Int, temporaryIndex: Int): List[TreeMap[Int, List[Int]]] = {

    if(disk == 1)
      movePegs(listOfPegs, sourceIndex, destinationIndex)

    else
    {
      val pegsAfterMove1 = pureRecursive(listOfPegs, disk - 1, sourceIndex, temporaryIndex, destinationIndex)
      val pegsAfterMove2 = pureRecursive(pegsAfterMove1, 1, sourceIndex, destinationIndex, temporaryIndex)
      pureRecursive(pegsAfterMove2, disk - 1, temporaryIndex, destinationIndex, sourceIndex)
    }
  }

  pureRecursive(getInitialPegMap(numberOfDisks), numberOfDisks, 0, (numberOfDisks % 2) + 1, 2 - (numberOfDisks % 2)).reverse
}

def startTailRecursive(numberOfDisks: Int) = {
  @tailrec
  def tailRecursive(listOfPegs: List[TreeMap[Int, List[Int]]], stepNumber: Int, numberOfDisks: Int): List[TreeMap[Int, List[Int]]] = {

    if(stepNumber == (1 << numberOfDisks))
      listOfPegs.reverse
    else
    {
      val result = movePegs(listOfPegs, (stepNumber & stepNumber - 1) % 3, ((stepNumber | stepNumber - 1) + 1) % 3)

      tailRecursive(result, stepNumber + 1, numberOfDisks)
    }
  }

  tailRecursive(getInitialPegMap(numberOfDisks), 1, numberOfDisks)
}

println("Pure Iterative")
printPegs(startIterativeTowerOfHanoi(5))

println("\nPure Recursive")
printPegs(startPureRecursive(5))

println("\nTail Recursive")
printPegs(startTailRecursive(5))