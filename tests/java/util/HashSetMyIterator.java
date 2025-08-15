/* Copyright (c) 2008-2015, Avian Contributors

   Permission to use, copy, modify, and/or distribute this software
   for any purpose with or without fee is hereby granted, provided
   that the above copyright notice and this permission notice appear
   in all copies.

   There is NO WARRANTY for this software.  See license.txt for
   details. */

package java.util;

public class HashSetMyIterator<T> implements Iterator<T> {
  private final Iterator<Entry<T, Object>> it;

  public HashSetMyIterator(Iterator<Entry<T, Object>> it) {
    this.it = it;
  }

  public T next() {
    return it.next().getKey();
  }

  public boolean hasNext() {
    return it.hasNext();
  }

  public void remove() {
    it.remove();
  }
}
