/* Copyright (c) 2008-2015, Avian Contributors

   Permission to use, copy, modify, and/or distribute this software
   for any purpose with or without fee is hereby granted, provided
   that the above copyright notice and this permission notice appear
   in all copies.

   There is NO WARRANTY for this software.  See license.txt for
   details. */

package java.util;

public class ArraysListIterator<T> implements ListIterator<T> {
  private final T[] array;
  private int currentIndex;

  public ArraysListIterator(T[] array, int index) {
    this.array = array;
    this.currentIndex = index;
  }

  @Override
  public boolean hasNext() {
    return currentIndex < array.length;
  }

  @Override
  public T next() {
    if (!hasNext()) {
      throw new java.util.NoSuchElementException();
    }
    return array[currentIndex++];
  }

  @Override
  public void remove() {
    throw new UnsupportedOperationException();
  }

  @Override
  public boolean hasPrevious() {
    return currentIndex > 0;
  }

  @Override
  public T previous() {
    if (!hasPrevious()) {
      throw new java.util.NoSuchElementException();
    }
    return array[--currentIndex];
  }

  public int nextIndex() {
    return currentIndex;
  }

  public int previousIndex() {
    return currentIndex - 1;
  }

  public void set(T e) {
    throw new UnsupportedOperationException();
  }

  public void add(T e) {
    throw new UnsupportedOperationException();
  }
}
