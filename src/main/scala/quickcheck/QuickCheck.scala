package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = { 
    for{ 
      x <- arbitrary[A]
      m <- oneOf(const(empty), genHeap)
    } yield insert(x, m)
  }
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }
  
  property("gen2") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    val h2 = deleteMin(h)
    val n = if (isEmpty(h2)) 0 else findMin(h2)
    val testList = List(m, n)
    val sortedList = testList.sorted
    val test = insert(m, empty)
    val test1 = insert(n, test)
    //println(findMin(test1))
   // println(sortedList)
    findMin(test1) == sortedList.head
    
  }
  
    property("gen3") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    val test = insert(m, empty)
    val test1 = insert(m+1, test)
    val test2 = deleteMin(test1)
    !isEmpty(test2)
  
  }
    
   property("gen4") = forAll { (h: H) =>
    def helper(h1: H, agg: List[A]): List[A]={
      if (isEmpty(h1)) return agg 
      else {
        val temp = findMin(h1)
        helper(deleteMin(h1), temp::agg)
      }
    }
   
    val m = helper(h, List())
    
    val n = m.sorted.reverse
    println(m)
    println(n)
    m==n
    
  }
   
    property("gen5") = forAll { (h: H) =>
    
      def helper(h1: H, agg: List[A]): List[A]={
      if (isEmpty(h1)) return agg 
      else {
        val temp = findMin(h1)
        helper(deleteMin(h1), temp::agg)
      }
    }
   
    val m = helper(h, List())
    
    val n = m.sorted
    
    def helper2(h2: H, elementsToAddList: List[A]) : H = {
      if (elementsToAddList.isEmpty) h2
      else {
        val temp3 = if (elementsToAddList.head< 2147483647) 1+elementsToAddList.head else elementsToAddList.head
        helper2(insert(temp3, h2), elementsToAddList.tail)
      }
    }
   // println("...")
   // println(h)
    
    val temp = helper2(empty, n)
   // println(temp)
    val temp2 = meld(h, temp)
   // println(temp2)
   // println(findMin(temp2))
   // println(findMin(h))
   // println(findMin(temp2)==findMin(h))
   // println("...")
    findMin(temp2)==findMin(h)
    
  }
    
    property("gen6") = forAll { (h: H) =>
    
      def helper(h1: H, agg: List[A]): List[A]={
      if (isEmpty(h1)) return agg 
      else {
        val temp = findMin(h1)
        helper(deleteMin(h1), temp::agg)
      }
    }
   
    val m = helper(h, List())
    
    val n = m.sorted
    
    def helper2(h2: H, elementsToAddList: List[A]) : H = {
      if (elementsToAddList.isEmpty) h2
      else {
        val temp3 = if (elementsToAddList.head< 2147483647) 1+elementsToAddList.head else elementsToAddList.head
        helper2(insert(temp3, h2), elementsToAddList.tail)
      }
    }
    //println("...")
   // println(h)
    
    val temp = helper2(empty, n)
   // println(temp)
    val temp2 = meld(temp, h)
    //println(temp2)
    //println(findMin(temp2))
   // println(findMin(h))
    //println(findMin(temp2)==findMin(h))
   // println("...")
    findMin(temp2)==findMin(h)
    
  }
    
    property("gen7") = forAll { (h: H) =>
  val l = insert (4, empty)
   val j = insert(1, l)
   val k = insert(2, j)
    findMin(k) == 1
  }
    
       
    property("gen8") = forAll { (h: H) =>
  val l = insert (4, empty)
   isEmpty(deleteMin(l))
   
  }
    
    property("gen9") = forAll { (h: H) =>
   val l = insert (4, empty)
   val j = insert(1, l)
   val k = insert(2, j)
   
   def helper(h1: H, agg: List[A]): List[A]={
      if (isEmpty(h1)) return agg 
      else {
        val temp = findMin(h1)
        helper(deleteMin(h1), temp::agg)
      }
    }
   
    val m = helper(k, List())
    
    val n = m.sorted.reverse
   
   
   
   m == List(4, 2, 1)
  }
    
    

}
