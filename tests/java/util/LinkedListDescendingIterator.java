/* Copyright (c) 2008-2015, Avian Contributors

   Permission to use, copy, modify, and/or distribute this software
   for any purpose with or without fee is hereby granted, provided
   that the above copyright notice and this permission notice appear
   in all copies.

   There is NO WARRANTY for this software.  See license.txt for
   details. */

package java.util;

public class LinkedListDescendingIterator<T> implements Iterator<T> {
  private final ListIterator<T> listIterator;

  public LinkedListDescendingIterator(ListIterator<T> listIterator) {
    this.listIterator = listIterator;
  }

  @Override
  public T next() {
    return listIterator.previous();
  }

  @Override
  public boolean hasNext() {
    return listIterator.hasPrevious();
  }

  @Override
  public void remove() {
    listIterator.remove();
  }
}
