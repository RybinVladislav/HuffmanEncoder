abstract class HuffmanTree

case class Fork(left: HuffmanTree, right: HuffmanTree, chars: List[Char], weight: Int) extends HuffmanTree

case class Leaf(char: Char, weight: Int) extends HuffmanTree

object Huffman {

  type Bit = Int

  def weight(tree: HuffmanTree) = tree match {
    case Leaf(char, weight) => weight
    case Fork(left, right, chars, weight) => weight
  }

  def chars(tree: HuffmanTree): List[Char] = tree match {
    case Leaf(char, weight) => List(char)
    case Fork(left, right, chars, weight) => chars
  }

  def makeCodeTree(left: HuffmanTree, right: HuffmanTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))

  def times(chars: List[Char]): List[(Char, Int)] = {
    var freqList: List[(Char, Int)] = Nil
    chars.toSet[Char].foreach((char) => {
      freqList = freqList ::: List((char, chars.count(c => c == char)))
    })
    freqList.sortBy(_._2)
  }

  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = freqs.map(tuple => Leaf(tuple._1, tuple._2))

  def singleton(trees: List[HuffmanTree]): Boolean = if (trees.length == 1) true else false

  def combine(trees: List[HuffmanTree]): List[HuffmanTree] = trees match {
      case first :: second :: tail => makeCodeTree(first, second) :: tail
  }

  def until[T](condition: T => Boolean, action: T => T)(data: T): T = if (condition(data)) data else until(condition, action)(action(data))

  def generateCodeTree(chars: List[Char]): HuffmanTree = until(singleton, combine)(makeOrderedLeafList(times(chars))).head

  def decode(tree: HuffmanTree, bits: List[Bit]): List[Char] = {

    def decodeAccumulator(originalTree: HuffmanTree, tree: HuffmanTree, bits: List[Bit], message: List[Char]): List[Char] = tree match {
      case Leaf(char, _) =>
        bits match {
          case head :: tail => decodeNextCharacter(originalTree, head :: tail, message ++ List(char))
          case _ => message ++ List(char)
        }
      case Fork(left, right, _, _) =>
        bits match {
          case 0 :: tail => decodeAccumulator(originalTree, left, tail, message)
          case 1 :: tail => decodeAccumulator(originalTree, right, tail, message)
          case _ => message
        }
    }

    def decodeNextCharacter(originalTree: HuffmanTree, bits: List[Bit], message: List[Char]): List[Char] = decodeAccumulator(originalTree, originalTree, bits, message)

    if (bits.isEmpty) List()
    else decodeAccumulator(tree, tree, bits, List())
  }

}