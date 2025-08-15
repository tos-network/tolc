/* Copyright (c) 2008-2015, Avian Contributors

   Permission to use, copy, modify, and/or distribute this software
   for any purpose with or without fee is hereby granted, provided
   that the above copyright notice and this permission notice appear
   in all copies.

   There is NO WARRANTY for this software.  See license.txt for
   details. */

package java.util;

public class UnmodifiableIterator<T> implements Iterator<T> {
  private final Iterator<T> inner;
  
  UnmodifiableIterator(Iterator<T> inner) {
    this.inner = inner;
  }
  
  @Override
  public T next() {
    return inner.next();
  }

  @Override
  public boolean hasNext() {
    return inner.hasNext();
  }

  @Override
  public void remove() {
    throw new UnsupportedOperationException();
  }
}
