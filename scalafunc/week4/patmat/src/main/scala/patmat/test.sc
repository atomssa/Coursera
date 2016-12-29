
import patmat.Huffman
import patmat.Huffman.{chars, _}

val sampleTree = makeCodeTree(
  makeCodeTree(Leaf('x', 1), Leaf('e', 1)),
  Leaf('t', 2)
)
val p = "abcda"
val charsList = string2Chars(p)
val code = createCodeTree(charsList: List[Char])

type Bit = Int



def choose(b : Int, tree : CodeTree): CodeTree ={
  tree match{
    case Fork(left, right, chars, weight) => if(b==0) left else right
    case Leaf(chars, weight) => tree
  }
}


def decode(tree : CodeTree, bits : List[Bit]) : List[Char]={
  def loop(bit : Bit, tail : List[Bit], code : CodeTree) : List[Char]={

    val res = code match{
      case Fork(left, right, chars, weight) => if(bit == 0) left else right
      case Leaf(chars, weight) => code
    }
    res match {
      case Leaf(c, w) => {
        if(tail.isEmpty) List(c) else List(c) ::: loop(tail.head, tail.tail, tree)
      }
      case Fork(l, r, c, w) => {
        if(tail.isEmpty) throw new Error("couldn't find char")
        else loop(tail.head, tail.tail, res)
      }
    }


  }
  if(bits.isEmpty) Nil else loop(bits.head, bits.tail, tree)

}
decode(code,List(0,1,0,1,1,0))

decode(frenchCode, secret)


//
def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
  def found(charList : List[Char] , c : Char) : Boolean = {

    charList match {
      case List() => false
      case h :: tail => if (h == c) true else found(tail, c)
    }
  }
  def findPattern(cList : List[Char], code : CodeTree) : List[Bit] = {
    if (cList.isEmpty) List()
    else {
      val c = cList.head
      code match {
        case Fork(left, right, chars, weight) => if (found(Huffman.chars(left), c)) 0 :: findPattern(cList, left) else 1 :: findPattern(cList, right)
        case Leaf(chars, weight) => findPattern(cList.tail, tree)
      }
    }
  }
  if(text.isEmpty) Nil else findPattern(text, tree )
}
encode(code)( string2Chars("adab"))

type CodeTable = List[(Char, List[Bit])]

def codeBits(table: CodeTable)(char: Char): List[Bit] = table match {
  case List() => List()
  case h::tail => val (c , bitList ) = h
    if(c == char) bitList else codeBits(tail) (char)
}

def convert(tree: CodeTree): CodeTable = {

  def loop(tree: CodeTree, bitList: List[Bit]): List[(Char, List[Bit])] = {
    tree match {
      case Leaf(char, weight) => List((char, bitList))
      case Fork(left, right, chars, weight) => loop(right, bitList ::: List(1)) ::: loop(left, bitList ::: List(0))
    }

  }

  loop(code, Nil)
}
convert(code)