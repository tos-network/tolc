/* Copyright (c) 2008-2015, Avian Contributors

   Permission to use, copy, modify, and/or distribute this software
   for any purpose with or without fee is hereby granted, provided
   that the above copyright notice and this permission notice appear
   in all copies.

   There is NO WARRANTY for this software.  See license.txt for
   details. */

package java.util;

import java.lang.reflect.Array;

public class Arrays {
  private Arrays() { }

  public static String toString(Object[] a) {
    return asList(a).toString();
  }

  public static String toString(boolean[] a) {
    if (a == null) {
      return "null";
    } else {
      StringBuilder sb = new StringBuilder();
      sb.append("[");
      for (int i = 0; i < a.length; ++i) {
        sb.append(String.valueOf(a[i]));
        if (i + 1 != a.length) {
          sb.append(", ");
        }
      }
      sb.append("]");
      return sb.toString();
    }
  }

  public static String toString(byte[] a) {
    if (a == null) {
      return "null";
    } else {
      StringBuilder sb = new StringBuilder();
      sb.append("[");
      for (int i = 0; i < a.length; ++i) {
        sb.append(String.valueOf(a[i]));
        if (i + 1 != a.length) {
          sb.append(", ");
        }
      }
      sb.append("]");
      return sb.toString();
    }
  }

  public static String toString(short[] a) {
    if (a == null) {
      return "null";
    } else {
      StringBuilder sb = new StringBuilder();
      sb.append("[");
      for (int i = 0; i < a.length; ++i) {
        sb.append(String.valueOf(a[i]));
        if (i + 1 != a.length) {
          sb.append(", ");
        }
      }
      sb.append("]");
      return sb.toString();
    }
  }

  public static String toString(int[] a) {
    if (a == null) {
      return "null";
    } else {
      StringBuilder sb = new StringBuilder();
      sb.append("[");
      for (int i = 0; i < a.length; ++i) {
        sb.append(String.valueOf(a[i]));
        if (i + 1 != a.length) {
          sb.append(", ");
        }
      }
      sb.append("]");
      return sb.toString();
    }
  }

  public static String toString(long[] a) {
    if (a == null) {
      return "null";
    } else {
      StringBuilder sb = new StringBuilder();
      sb.append("[");
      for (int i = 0; i < a.length; ++i) {
        sb.append(String.valueOf(a[i]));
        if (i + 1 != a.length) {
          sb.append(", ");
        }
      }
      sb.append("]");
      return sb.toString();
    }
  }



  private static boolean equal(Object a, Object b) {
    return (a == null && b == null) || (a != null && a.equals(b));
  }

  public static void sort(Object[] array) {
    sort(array, new ArraysComparator());
  }

  private final static int SORT_SIZE_THRESHOLD = 16;

  public static <T> void sort(T[] array, Comparator<? super T> comparator) {
    introSort(array, comparator, 0, array.length, array.length);
    insertionSort(array, comparator);
  }

  private static <T > void introSort(T[] array,
    Comparator<? super T> comparator, int begin, int end, int limit)
  {
    while (end - begin > SORT_SIZE_THRESHOLD) {
      if (limit == 0) {
        heapSort(array, comparator, begin, end);
        return;
      }
      limit >>= 1;

      // For blockchain VM, use deterministic pivot selection
      // Always choose the middle element as pivot for consistency
      int pivotIndex = begin + (end - begin) / 2;
      T pivot = array[pivotIndex];
      
      // Move pivot to end for partitioning
      T temp = array[end - 1];
      array[end - 1] = pivot;
      array[pivotIndex] = temp;

      // partition
      int i = begin, j = end - 1;
      while (i < j) {
        while (i < j && comparator.compare(array[i], pivot) <= 0) {
          ++i;
        }
        while (i < j && comparator.compare(pivot, array[j]) <= 0) {
          --j;
        }
        if (i < j) {
          T swap = array[i];
          array[i] = array[j];
          array[j] = swap;
        }
      }

      // Move pivot back to its final position
      temp = array[i];
      array[i] = pivot;
      array[end - 1] = temp;

      introSort(array, comparator, i + 1, end, limit);
      end = i;
    }
  }

  private static <T> void heapSort(T[] array, Comparator<? super T> comparator,
    int begin, int end)
  {
    int count = end - begin;
    for (int i = count / 2 - 1; i >= 0; --i) {
      siftDown(array, comparator, i, count, begin);
    }
    for (int i = count - 1; i > 0; --i) {
      // swap begin and begin + i
      T swap = array[begin + i];
      array[begin + i] = array[begin];
      array[begin] = swap;

      siftDown(array, comparator, 0, i, begin);
    }
  }

  private static <T> void siftDown(T[] array, Comparator<? super T> comparator,
    int i, int count, int offset)
  {
    T value = array[offset + i];
    while (i < count / 2) {
      int child = 2 * i + 1;
      if (child + 1 < count &&
          comparator.compare(array[child], array[child + 1]) < 0) {
        ++child;
      }
      if (comparator.compare(value, array[child]) >= 0) {
        break;
      }
      array[offset + i] = array[offset + child];
      i = child;
    }
    array[offset + i] = value;
  }

  private static <T> void insertionSort(T[] array,
    Comparator<? super T> comparator)
  {
    for (int j = 1; j < array.length; ++j) {
      T t = array[j];
      int i = j - 1;
      while (i >= 0 && comparator.compare(array[i], t) > 0) {
        array[i + 1] = array[i];
        i = i - 1;
      }
      array[i + 1] = t;
    }
  }

  public static int hashCode(Object[] array) {
    if(array == null) {
      return 9023;
    }

    int hc = 823347;
    for(Object o : array) {
      hc += o != null ? o.hashCode() : 54267;
      hc *= 3;
    }
    return hc;
  }

  public static int hashCode(byte[] array) {
    if(array == null) {
      return 9023;
    }

    int hc = 823347;
    for(byte b : array) {
      hc += b;
      hc *= 3;
    }
    return hc;
  }

  public static boolean equals(Object[] a, Object[] b) {
    if(a == b) {
      return true;
    }
    if(a == null || b == null) {
      return false;
    }
    if(a.length != b.length) {
      return false;
    }
    for(int i = 0; i < a.length; i++) {
      if(!equal(a[i], b[i])) {
        return false;
      }
    }
    return true;
  }

  public static boolean equals(byte[] a, byte[] b) {
    if(a == b) {
      return true;
    }
    if(a == null || b == null) {
      return false;
    }
    if(a.length != b.length) {
      return false;
    }
    for(int i = 0; i < a.length; i++) {
      if(a[i] != b[i]) {
        return false;
      }
    }
    return true;
  }

  public static boolean equals(int[] a, int[] b) {
    if(a == b) {
      return true;
    }
    if(a == null || b == null) {
      return false;
    }
    if(a.length != b.length) {
      return false;
    }
    for(int i = 0; i < a.length; i++) {
      if(a[i] != b[i]) {
        return false;
      }
    }
    return true;
  }

  public static boolean equals(long[] a, long[] b) {
    if(a == b) {
      return true;
    }
    if(a == null || b == null) {
      return false;
    }
    if(a.length != b.length) {
      return false;
    }
    for(int i = 0; i < a.length; i++) {
      if(a[i] != b[i]) {
        return false;
      }
    }
    return true;
  }

  public static boolean equals(short[] a, short[] b) {
    if(a == b) {
      return true;
    }
    if(a == null || b == null) {
      return false;
    }
    if(a.length != b.length) {
      return false;
    }
    for(int i = 0; i < a.length; i++) {
      if(a[i] != b[i]) {
        return false;
      }
    }
    return true;
  }

  public static boolean equals(char[] a, char[] b) {
    if(a == b) {
      return true;
    }
    if(a == null || b == null) {
      return false;
    }
    if(a.length != b.length) {
      return false;
    }
    for(int i = 0; i < a.length; i++) {
      if(a[i] != b[i]) {
        return false;
      }
    }
    return true;
  }



  public static boolean deepEquals(Object[] a, Object[] b) {
    if(a == b) {
      return true;
    }
    if(a == null || b == null) {
      return false;
    }
    if(a.length != b.length) {
      return false;
    }
    for(int i = 0; i < a.length; i++) {
      if(!Objects.deepEquals(a[i], b[i])) {
        return false;
      }
    }
    return true;
  }

  public static <T> List<T> asList(final T ... array) {
    return new ArraysAbstractList<T>(array);
  }

  private static void checkRange(int len, int start, int stop) {
    if (start < 0) {
      throw new ArrayIndexOutOfBoundsException(start);
    }
    if (stop > len) {
      throw new ArrayIndexOutOfBoundsException(stop);
    }
    if (start > stop) {
      throw new IllegalArgumentException("start(" + start + ") > stop(" + stop + ")");
    }
  }

  public static void fill(int[] array, int value) {
    for (int i=0;i<array.length;i++) {
      array[i] = value;
    }
  }

  public static void fill(int[] array, int start, int stop, int value) {
    checkRange(array.length, start, stop);
    for (int i=start;i<stop;i++) {
      array[i] = value;
    }
  }

  public static void fill(char[] array, char value) {
    for (int i=0;i<array.length;i++) {
      array[i] = value;
    }
  }

  public static void fill(char[] array, int start, int stop, char value) {
    checkRange(array.length, start, stop);
    for (int i=start;i<stop;i++) {
      array[i] = value;
    }
  }

  public static void fill(short[] array, short value) {
    for (int i=0;i<array.length;i++) {
      array[i] = value;
    }
  }

  public static void fill(short[] array, int start, int stop, short value) {
    checkRange(array.length, start, stop);
    for (int i=start;i<stop;i++) {
      array[i] = value;
    }
  }

  public static void fill(byte[] array, byte value) {
    for (int i=0;i<array.length;i++) {
      array[i] = value;
    }
  }
  
  public static void fill(byte[] array, int start, int stop, byte value) {
    checkRange(array.length, start, stop);
    for (int i=start;i<stop;i++) {
      array[i] = value;
    }
  }

  public static void fill(boolean[] array, boolean value) {
    for (int i=0;i<array.length;i++) {
      array[i] = value;
    }
  }

  public static void fill(boolean[] array, int start, int stop, boolean value) {
    checkRange(array.length, start, stop);
    for (int i=start;i<stop;i++) {
      array[i] = value;
    }
  }

  public static void fill(long[] array, long value) {
    for (int i=0;i<array.length;i++) {
      array[i] = value;
    }
  }

  public static void fill(long[] array, int start, int stop, long value) {
    checkRange(array.length, start, stop);
    for (int i=start;i<stop;i++) {
      array[i] = value;
    }
  }



  public static <T> void fill(T[] array, T value) {
    for (int i=0;i<array.length;i++) {
      array[i] = value;
    }
  }

  public static <T> void fill(T[] array, int start, int stop, T value) {
    checkRange(array.length, start, stop);
    for (int i=start;i<stop;i++) {
      array[i] = value;
    }
  }

  public static boolean[] copyOf(boolean[] array, int newLength) {
    boolean[] result = new boolean[newLength];
    int length = array.length > newLength ? newLength : array.length;
    System.arraycopy(array, 0, result, 0, length);
    return result;
  }

  public static byte[] copyOf(byte[] array, int newLength) {
    byte[] result = new byte[newLength];
    int length = array.length > newLength ? newLength : array.length;
    System.arraycopy(array, 0, result, 0, length);
    return result;
  }

  public static char[] copyOf(char[] array, int newLength) {
    char[] result = new char[newLength];
    int length = array.length > newLength ? newLength : array.length;
    System.arraycopy(array, 0, result, 0, length);
    return result;
  }



  public static int[] copyOf(int[] array, int newLength) {
    int[] result = new int[newLength];
    int length = array.length > newLength ? newLength : array.length;
    System.arraycopy(array, 0, result, 0, length);
    return result;
  }

  public static long[] copyOf(long[] array, int newLength) {
    long[] result = new long[newLength];
    int length = array.length > newLength ? newLength : array.length;
    System.arraycopy(array, 0, result, 0, length);
    return result;
  }

  public static short[] copyOf(short[] array, int newLength) {
    short[] result = new short[newLength];
    int length = array.length > newLength ? newLength : array.length;
    System.arraycopy(array, 0, result, 0, length);
    return result;
  }

  public static <T> T[] copyOf(T[] array, int newLength) {
    Class<?> clazz = array.getClass().getComponentType();
    T[] result = (T[])Array.newInstance(clazz, newLength);
    int length = array.length > newLength ? newLength : array.length;
    System.arraycopy(array, 0, result, 0, length);
    return result;
  }

  public static <T, U> T[] copyOf(U[] array, int newLength,
    Class<? extends T[]> newType)
  {
    T[] result = (T[])Array.newInstance(newType.getComponentType(), newLength);
    int length = array.length > newLength ? newLength : array.length;
    System.arraycopy(array, 0, result, 0, length);
    return result;
  }

  // copyOfRange methods
  public static boolean[] copyOfRange(boolean[] original, int from, int to) {
    int newLength = to - from;
    if (newLength < 0)
      throw new IllegalArgumentException(from + " > " + to);
    boolean[] copy = new boolean[newLength];
    System.arraycopy(original, from, copy, 0,
                     Math.min(original.length - from, newLength));
    return copy;
  }

  public static byte[] copyOfRange(byte[] original, int from, int to) {
    int newLength = to - from;
    if (newLength < 0)
      throw new IllegalArgumentException(from + " > " + to);
    byte[] copy = new byte[newLength];
    System.arraycopy(original, from, copy, 0,
                     Math.min(original.length - from, newLength));
    return copy;
  }

  public static char[] copyOfRange(char[] original, int from, int to) {
    int newLength = to - from;
    if (newLength < 0)
      throw new IllegalArgumentException(from + " > " + to);
    char[] copy = new char[newLength];
    System.arraycopy(original, from, copy, 0,
                     Math.min(original.length - from, newLength));
    return copy;
  }



  public static int[] copyOfRange(int[] original, int from, int to) {
    int newLength = to - from;
    if (newLength < 0)
      throw new IllegalArgumentException(from + " > " + to);
    int[] copy = new int[newLength];
    System.arraycopy(original, from, copy, 0,
                     Math.min(original.length - from, newLength));
    return copy;
  }

  public static long[] copyOfRange(long[] original, int from, int to) {
    int newLength = to - from;
    if (newLength < 0)
      throw new IllegalArgumentException(from + " > " + to);
    long[] copy = new long[newLength];
    System.arraycopy(original, from, copy, 0,
                     Math.min(original.length - from, newLength));
    return copy;
  }

  public static short[] copyOfRange(short[] original, int from, int to) {
    int newLength = to - from;
    if (newLength < 0)
      throw new IllegalArgumentException(from + " > " + to);
    short[] copy = new short[newLength];
    System.arraycopy(original, from, copy, 0,
                     Math.min(original.length - from, newLength));
    return copy;
  }

  public static <T> T[] copyOfRange(T[] original, int from, int to) {
    return copyOfRange(original, from, to, (Class<? extends T[]>) original.getClass());
  }

  public static <T, U> T[] copyOfRange(U[] original, int from, int to,
                                       Class<? extends T[]> newType) {
    int newLength = to - from;
    if (newLength < 0)
      throw new IllegalArgumentException(from + " > " + to);
    T[] copy = (T[]) Array.newInstance(newType.getComponentType(), newLength);
    System.arraycopy(original, from, copy, 0,
                     Math.min(original.length - from, newLength));
    return copy;
  }

  public static int binarySearch(int[] a, int key) {
    return binarySearch(a, 0, a.length - 1, key);
  }

  public static int binarySearch(int[] a, int fromIndex, int toIndex, int key) {
    checkRange(a.length, fromIndex, toIndex);

    // Assume array is already sorted
    int left = fromIndex;
    int right = toIndex - 1;
    int mid;
    while(left <= right) {
      mid = (left + right) / 2;
      if(a[mid] < key) {
        left = mid + 1;
      } else if(a[mid] > key) {
        right = mid - 1;
      } else {
        return mid;
      }
    }

    return -left - 1;
  }

}
