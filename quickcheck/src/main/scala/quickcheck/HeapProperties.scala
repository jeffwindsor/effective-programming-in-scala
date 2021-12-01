package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.*

import scala.annotation.tailrec

trait HeapProperties(val heapInterface: HeapInterface):

  // Import all the operations of the `HeapInterface` (e.g., `empty`
  // `insert`, etc.)
  import heapInterface.*

  val minOfTwo: (String, Prop) =
    "the minimum of a heap of two elements should be the smallest of the two elements" ->
    forAll { (x1: Int, x2: Int) =>
      val heap = insert(x2, insert(x1, empty))
      findMin(heap) == ( x1 min x2 )
    }

  val deleteMinOfOne: (String, Prop) =
    "delete minimum of heap of one element should return an empty heap" ->
    forAll { (x: Int) =>
      // create a heap with exactly one element, `x`
      val heap1: List[Node] = insert(x, empty)
      // delete the minimal element from it
      val heap0: List[Node] = deleteMin(heap1)
      // check that heap0 is empty
      isEmpty(heap0)
    }

  val insertMinAndGetMin: (String, Prop) =
    "inserting the minimal element and then finding it should return the same minimal element" ->
    forAll(generatedHeap.suchThat(heap => !isEmpty(heap))) { (heap: List[Node]) =>
      // find the minimimal element of the heap
      // (you don’t need to handle the case of empty heaps because it has been excluded from the heap generator)
      val min: Int = findMin(heap)
      // insert the minimal element to the heap
      val updatedHeap: List[Node] = insert(min, heap)
      // find the minimal element of the updated heap should return the same minimal element
      findMin(updatedHeap) == min
    }

  //SHOULD FAIL: List(Node(1207187240,0,List()), Node(667182096,1,List(Node(2147483647,0,List()))))
  val deleteAllProducesSortedList: (String, Prop) =
    // recursively traverse the heap
    @tailrec
    def check(heap: List[Node]): Boolean =
      // if the heap is empty, or if it has just one element, we have
      // successfully finished our checks
      if isEmpty(heap) || isEmpty(deleteMin(heap)) then
        true
      else
        // find the minimal element
        val x1 = findMin(heap)
        // delete the minimal element of `heap`
        val heap2 = deleteMin(heap)
        // find the minimal element in `heap2`
        val x2 = findMin(heap2)
        // check that the deleted element is less than or equal to the
        // minimal element of the remaining heap, and that the remaining
        // heap verifies the same property (by recursively calling `check`)
        if x1 <= x2 then check(heap2)
        else false

    // check arbitrary heaps
    "continually finding and deleting the minimal element of a heap should return a sorted sequence" ->
    forAll { (heap: List[Node]) =>
      check(heap)
    }

  val meldingSmallHeaps: (String, Prop) =
    "melding a heap containing two low values with a heap containing two high values" ->
    forAll { (x: Int, y: Int) =>
      // create two heaps:
      // - the first has two duplicate elements inserted, where both are equal to the
      //   highest value among `x` and `y`
      val minXY = x min y
      val maxXY = x max y
      val minHeap = insert(minXY, insert(minXY, empty))
      val maxHeap = insert(maxXY, insert(maxXY, empty))
      // - the second also has two duplicate elements insterted, where both are equal
      //   to the lowest value among `x` and `y`
      // finally, meld both heaps.
      val meldedHeap: List[Node] =  meld(maxHeap, minHeap)

      // check that deleting the minimal element twice in a row from the melded heap,
      // and then finding the minimal element in the resulting heap returns the
      // highest value
      val deleteTwoMinAndFindMin: Boolean =
        val deleteTwoHeap = deleteMin(deleteMin(meldedHeap))
        maxXY == findMin(deleteTwoHeap)

      // check that inserting the lowest value to the melded heap, and then
      // finding the minimal element returns the lowest value
      val insertMinAndFindMin: Boolean =
        val insertedHeap = insert(minXY, meldedHeap)
        minXY == findMin(insertedHeap)

      // check that both conditions are fulfilled
      deleteTwoMinAndFindMin && insertMinAndFindMin
    }

  // Given two arbitrary heaps, and the heap that results from melding
  // them together, finding the minimum of the melded heap should return
  // the minimum of the two source heaps. Then, continuously deleting
  // that minimum element (from both the melded heap and the source heap
  // that contained it) should always give back a melded heap whose
  // minimum element is the minimum element of one of the two source
  // heaps, until the two source heaps are empty.
  //
  // Hint 1: write an auxiliary (recursive) method checking that the melded
  // heap is valid with respect to its two source heaps.
  //
  // Hint 2: that auxiliary method should handle four cases:
  //  1. the melded heap is empty (which should happen only if the two source
  //     heaps were empty),
  //  2. the minimum of the melded heap is the minimum of the first source
  //     heap (then, check that after removing the minimum from the melded
  //     heap and from the first source heap, the resulting heaps are still
  //     valid),
  //  3. the minimum of the melded heap is the minimum of the second source
  //     heap (then, check that after removing the minimum from the melded
  //     heap and from the second source heap, the resulting heaps are still
  //     valid),
  //  4. all the other cases (which should not happen in correct heap
  //     implementations).
  val meldingHeaps: (String, Prop) =
    "finding the minimum of melding any two heaps should return the minimum of one or the other of the source heaps" ->
    forAll { (heap1: List[Node], heap2: List[Node]) =>

      @tailrec
      def inner(heap1: List[Node], heap2: List[Node], melded: List[Node]): Boolean =
        if isEmpty(melded) && isEmpty(heap1) && isEmpty(heap2) then true
        else if !isEmpty(heap1) && findMin(melded) == findMin(heap1) then inner(deleteMin(heap1), heap2, deleteMin(melded))
        else if !isEmpty(heap2) && findMin(melded) == findMin(heap2) then inner(heap1, deleteMin(heap2), deleteMin(melded))
        else false

      inner(heap1, heap2, meld(heap1, heap2))
    }

  // Random heap generator (used by Scalacheck)
  given generatedHeap: Gen[List[Node]]
  given Arbitrary[List[Node]] = Arbitrary(generatedHeap)

end HeapProperties

