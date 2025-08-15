/* Copyright (c) 2008-2015, Avian Contributors

   Permission to use, copy, modify, and/or distribute this software
   for any purpose with or without fee is hereby granted, provided
   that the above copyright notice and this permission notice appear
   in all copies.

   There is NO WARRANTY for this software.  See license.txt for
   details. */

package java.util;

public class ArraysAbstractList<T> extends AbstractList<T> {
  private final T[] array;

  public ArraysAbstractList(T[] array) {
    this.array = array;
  }

  @Override
  public int size() {
    return array.length;
  }

  @Override
  public void add(int index, T element) {
    throw new UnsupportedOperationException();
  }

  private static boolean equal(Object a, Object b) {
    return (a == null && b == null) || (a != null && a.equals(b));
  }

  @Override
  public int indexOf(Object element) {
    for (int i = 0; i < array.length; ++i) {
      if (equal(element, array[i])) {
        return i;
      }
    }
    return -1;
  }

  @Override
  public int lastIndexOf(Object element) {
    for (int i = array.length - 1; i >= 0; --i) {
      if (equal(element, array[i])) {
        return i;
      }
    }
    return -1;
  }

  @Override
  public T get(int index) {
    return array[index];
  }

  @Override
  public T set(int index, T value) {
    throw new UnsupportedOperationException();
  }

  @Override
  public T remove(int index) {
    throw new UnsupportedOperationException();
  }

  @Override
  public ListIterator<T> listIterator(int index) {
    return new ArraysListIterator<>(array, index);
  }
}
