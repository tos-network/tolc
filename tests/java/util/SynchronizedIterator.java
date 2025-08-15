/* Copyright (c) 2008-2015, Avian Contributors

   Permission to use, copy, modify, and/or distribute this software
   for any purpose with or without fee is hereby granted, provided
   that the above copyright notice and this permission notice appear
   in all copies.

   There is NO WARRANTY for this software.  See license.txt for
   details. */

package java.util;

public class SynchronizedIterator<T> implements Iterator<T> {
  private final Object lock;
  private final Iterator<T> it;

  public SynchronizedIterator(Object lock, Iterator<T> it) {
    this.lock = lock;
    this.it = it;
  }

  public T next() {
    // Simplified for blockchain VM - no synchronization needed
    return it.next();
  }

  public boolean hasNext() {
    // Simplified for blockchain VM - no synchronization needed
    return it.hasNext();
  }

  public void remove() {
    // Simplified for blockchain VM - no synchronization needed
    it.remove();
  }
}
