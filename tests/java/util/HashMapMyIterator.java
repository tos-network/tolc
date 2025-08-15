/* Copyright (c) 2008-2015, Avian Contributors

   Permission to use, copy, modify, and/or distribute this software
   for any purpose with or without fee is hereby granted, provided
   that the above copyright notice and this permission notice appear
   in all copies.

   There is NO WARRANTY for this software.  See license.txt for
   details. */

package java.util;

import java.util.NoSuchElementException;

public class HashMapMyIterator<K, V> implements Iterator<Entry<K, V>> {
  private final HashMap<K, V> hashMap;
  private HashMapCell<K, V> next;  // Start from head of linked list

  public HashMapMyIterator(HashMap<K, V> hashMap) {
    this.hashMap = hashMap;
    this.next = hashMap.getHead();  // Assuming we add a getHead() method
  }

  public boolean hasNext() {
    return next != null;
  }

  public Entry<K, V> next() {
    if (next == null) throw new NoSuchElementException();
    HashMapCell<K, V> e = next;
    next = next.after();
    return e;
  }

  public void remove() {
    throw new UnsupportedOperationException("Use HashMap.remove(key) instead");
  }
}
