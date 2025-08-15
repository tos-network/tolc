/* Copyright (c) 2008-2015, Avian Contributors

   Permission to use, copy, modify, and/or distribute this software
   for any purpose with or without fee is hereby granted, provided
   that the above copyright notice and this permission notice appear
   in all copies.

   There is NO WARRANTY for this software.  See license.txt for
   details. */

package java.base;

import java.util.Map;
import java.util.Entry;
import java.util.Iterator;

public class DataValueIterator<K, V> implements Iterator<V> {
  private final Iterator<Entry<K, V>> it;

  public DataValueIterator(Iterator<Entry<K, V>> it) {
    this.it = it;
  }

  public V next() {
    return it.next().getValue();
  }

  public boolean hasNext() {
    return it.hasNext();
  }

  public void remove() {
    it.remove();
  }
}
