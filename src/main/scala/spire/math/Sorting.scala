package spire.math

import spire.algebra.Order
import spire.syntax.order.given

import scala.reflect.ClassTag

/**
 *  Interface for a sorting strategy object.
 */
trait Sort extends Any {
  def sort[@specialized A: Order: ClassTag](data: Array[A]): Unit
}

/**
 * An implementation of insertion sort.
 *
 * Works well for small arrays but due to quadratic complexity is not generally optimal.
 */
object InsertionSort extends Sort {

  /**
    * Sorts `data` in place using insertion sort.
    *
    * @param data the array to be sorted
    * @tparam A a member of the type class `Order`
    */
  final def sort[@specialized A: Order: ClassTag](data: Array[A]): Unit =
    sort(data, 0, data.length)

  /**
    * Uses insertion sort on `data` to sort the entries from the index `start`
    * up to, but not including, the index `end`. Operates in place.
    *
    * @param data the array to be sorted
    * @param start the index of the first element, inclusive, to be sorted
    * @param end the index of the last element, exclusive, to be sorted
    * @tparam A a member of the type class `Order`
    */
  final def sort[@specialized A: Order: ClassTag](data: Array[A], start: Int, end: Int): Unit = {
    require(start <= end && start >= 0 && end <= data.length)
    var i = start + 1
    while (i < end) {
      val item = data(i)
      var hole = i
      while (hole > start && data(hole - 1) > item) {
        data(hole) = data(hole - 1)
        hole -= 1
      }
      data(hole) = item
      i += 1
    }
  }
}

/**
 * In-place quicksort implementation. It is not stable, but does not allocate
 * extra space (other than stack). It uses InsertionSort for sorting very small arrays.
 */
object QuickSort {
  inline val limit: Int = 16

  /**
    * Uses quicksort on `data` to sort the entries. Operates in place.
    *
    * If the size of the input array is less than the threshold `limit`,
    * uses insertion sort instead.
    *
    * @param data the array to be sorted
    * @tparam A a member of the type class `Order`
    */
  final def sort[@specialized A: Order: ClassTag](data: Array[A]): Unit = qsort(data, 0, data.length)

  /**
    * Uses quicksort on `data` to sort the entries from the index `start`
    * up to, but not including, the index `end`. Operates in place.
    *
    * If the size of the segment to be sorted is less than the threshold `limit`
    * uses insertion sort instead.
    *
    * @param data the array to be sorted
    * @param start the index from which to start sorting (inclusive)
    * @param end the index at which to stop sorting (exclusive)
    * @tparam A a member of the type class `Order`
    */
  final def qsort[@specialized A: Order: ClassTag](data: Array[A], start: Int, end: Int): Unit = {
    require(start >= 0 && end <= data.length)
    if (end - start < limit) {
      InsertionSort.sort(data, start, end)
      return
    }

    val pivotIndex = start + (end - start) / 2
    val nextPivotIndex = partition(data, start, end, pivotIndex)
    qsort(data, start, nextPivotIndex)
    qsort(data, nextPivotIndex + 1, end)
  }

  /**
    * Helper method for the quick sort implementation. Partitions the segment of the array `data` from `start` to `end`
    * according to the value at the given `pivotIndex`. Values in the segment less than the pivot value will end up
    * to the left of the pivot value, and values greater on the right. Operates in place.
    *
    * @param data the array to be partitioned
    * @param start the left endpoint (inclusive) of the interval to be partitioned
    * @param end the right endpoint (exclusive) of the interval to be partitioned
    * @param pivotIndex the index of the current pivot
    * @tparam A a member of the type class Order
    * @return the next pivot value
    */
  final def partition[@specialized A: Order: ClassTag](data: Array[A], start: Int, end: Int, pivotIndex: Int): Int = {
    require(start >= 0 && pivotIndex >= start && end > pivotIndex && end <= data.length)
    val pivotValue = data(pivotIndex)

    data(pivotIndex) = data(end - 1)

    var temp = pivotValue
    var store = start
    var i = start
    while (i < end - 1) {
      if (data(i) < pivotValue) {
        //swap(i, store)
        temp = data(i); data(i) = data(store); data(store) = temp
        store += 1
      }
      i += 1
    }

    data(end - 1) = data(store)
    data(store) = pivotValue
    store
  }
}

// TODO: it would be nice to try implementing some hybrid sorts, for instance
// Tim Peters' sort algorithm.

/**
 * Object providing in-place sorting capability for arrays.
 *
 * Sorting.sort() uses quickSort() by default (in-place, not stable, generally
 * fastest but might hit bad cases where it is quadratic. Also provides
 * insertionSort(), which is slow except for small arrays.
 */
object Sorting {
  /** Delegates to [[spire.math.QuickSort.sort]] */
  final def sort[@specialized A: Order: ClassTag](data: Array[A]): Unit = QuickSort.sort(data)

  /** Delegates to [[spire.math.InsertionSort.sort]] */
  final def insertionSort[@specialized A: Order: ClassTag](data: Array[A]): Unit = InsertionSort.sort(data)

  /** Delegates to [[spire.math.QuickSort.sort]] */
  final def quickSort[@specialized A: Order: ClassTag](data: Array[A]): Unit = QuickSort.sort(data)
}
