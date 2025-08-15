/* Copyright (c) 2008-2015, Avian Contributors

   Permission to use, copy, modify, and/or distribute this software
   for any purpose with or without fee is hereby granted, provided
   that the above copyright notice and this permission notice appear
   in all copies.

   There is NO WARRANTY for this software.  See license.txt for
   details. */

package java.util;

import java.base.Data;

public class Collections {

  private Collections() { }

  public static void sort(List list) {
    sort(list, new CollectionsComparator());
  }

  private final static int SORT_SIZE_THRESHOLD = 16;

  public static <T> void sort(List<T> list, Comparator<? super T> comparator) {
    int size = list.size();
    introSort(list, comparator, 0, size, size);
    insertionSort(list, comparator);
  }

  private static <T > void introSort(List<T> list,
    Comparator<? super T> comparator, int begin, int end, int limit)
  {
    while (end - begin > SORT_SIZE_THRESHOLD) {
      if (limit == 0) {
        heapSort(list, comparator, begin, end);
        return;
      }
      limit >>= 1;

      // For blockchain VM, use deterministic pivot selection
      // Always choose the middle element as pivot for consistency
      int pivotIndex = begin + (end - begin) / 2;
      T pivot = list.get(pivotIndex);
      
      // Move pivot to end for partitioning
      T temp = list.get(end - 1);
      list.set(end - 1, pivot);
      list.set(pivotIndex, temp);

      // partition
      int i = begin, j = end - 1;
      while (i < j) {
        while (i < j && comparator.compare(list.get(i), pivot) <= 0) {
          ++i;
        }
        while (i < j && comparator.compare(pivot, list.get(j)) <= 0) {
          --j;
        }
        if (i < j) {
          T swap = list.get(i);
          list.set(i, list.get(j));
          list.set(j, swap);
        }
      }

      // Move pivot back to its final position
      temp = list.get(i);
      list.set(i, pivot);
      list.set(end - 1, temp);

      introSort(list, comparator, i + 1, end, limit);
      end = i;
    }
  }

  private static <T> void heapSort(List<T> list, Comparator<? super T> comparator,
    int begin, int end)
  {
    int count = end - begin;
    for (int i = count / 2 - 1; i >= 0; --i) {
      siftDown(list, comparator, i, count, begin);
    }
    for (int i = count - 1; i > 0; --i) {
      // swap begin and begin + i
      T swap = list.get(begin + i);
      list.set(begin + i, list.get(begin));
      list.set(begin, swap);

      siftDown(list, comparator, 0, i, begin);
    }
  }

  private static <T> void siftDown(List<T> list, Comparator<? super T> comparator,
    int i, int count, int offset)
  {
    T value = list.get(offset + i);
    while (i < count / 2) {
      int child = 2 * i + 1;
      if (child + 1 < count &&
          comparator.compare(list.get(child), list.get(child + 1)) < 0) {
        ++child;
      }
      if (comparator.compare(value, list.get(child)) >= 0) {
        break;
      }
      list.set(offset + i, list.get(offset + child));
      i = child;
    }
    list.set(offset + i, value);
  }

  private static <T> void insertionSort(List<T> list,
    Comparator<? super T> comparator)
  {
    int size = list.size();
    for (int j = 1; j < size; ++j) {
      T t = list.get(j);
      int i = j - 1;
      while (i >= 0 && comparator.compare(list.get(i), t) > 0) {
        list.set(i + 1, list.get(i));
        --i;
      }
      list.set(i + 1, t);
    }
  }

  public static <T> int binarySearch(List<T> list, T needle) {
    int left = -1, right = list.size();
    while (left + 1 < right) {
      int middle = (left + right) >> 1;
      int result = ((Comparable)needle).compareTo(list.get(middle));
      if (result < 0) {
        right = middle;
      } else if (result > 0) {
        left = middle;
      } else {
        return middle;
      }
    }
    return -1 - right;
  }

  public static <T> void reverse(List<T> list) {
    int ascending = 0, descending = list.size() - 1;
    while (ascending < descending) {
      T tmp = list.get(ascending);
      list.set(ascending++, list.get(descending));
      list.set(descending--, tmp);
    }
  }

  public static final List EMPTY_LIST
    = new UnmodifiableList<Object>(new ArrayList<Object>(0));

  public static final <E> List<E> emptyList() {
    return EMPTY_LIST;
  }

  public static final <K,V> Map<K,V> emptyMap() {
    return (Map<K, V>) new UnmodifiableMap<Object, Object>(
      new HashMap<Object, Object>(0));
  }

  public static final <T> Set<T> emptySet() {
    return (Set<T>) new UnmodifiableSet<Object>(
      new HashSet<Object>(0));
  }
  
  public static <T> Enumeration<T> enumeration(Collection<T> c) {
    return new IteratorEnumeration<T> (c.iterator());
  }

  public static <T> Comparator<T> reverseOrder(Comparator<T> cmp) {
    return new ReverseComparator<T>(cmp);
  }
    
  public static <K,V> Map<K,V> synchronizedMap(Map<K,V> map) {
    // Simplified for blockchain VM - return original map without synchronization
    return map; 
  }
  
  public static <V> Set<V> synchronizedSet(Set<V> set) {
    // Simplified for blockchain VM - return original set without synchronization
    return set;
  }
  
  public static <T> List<T> synchronizedList(List<T> list) {
    // Simplified for blockchain VM - return original list without synchronization
    return list;
  }

  public static <T> List<T> unmodifiableList(List<T> list)  {
    return new UnmodifiableList<T>(list);
  }

  public static <K,V> Map<K,V> unmodifiableMap(Map<K,V> m) {
	  return new UnmodifiableMap<K, V>(m);
  }
  
  public static <T> UnmodifiableCollection<T> unmodifiableCollection(Collection<T> collection) {
    return new UnmodifiableCollection<T>(collection);
  }

  public static <T> Set<T> unmodifiableSet(Set<T> hs) {
    return new UnmodifiableSet<T>(hs);
  }
  
  public static <T> List<T> singletonList(T o) {
    ArrayList<T> list = new ArrayList<T>(1);
    list.add(o);
    return new UnmodifiableList(list);
  }
}
