/* Copyright (c) 2008-2015, Avian Contributors

   Permission to use, copy, modify, and/or distribute this software
   for any purpose with or without fee is hereby granted, provided
   that the above copyright notice and this permission notice appear
   in all copies.

   There is NO WARRANTY for this software.  See license.txt for
   details. */

package java.base;

import java.util.AbstractSet;
import java.util.Iterator;

public class DataKeySet<K> extends AbstractSet<K> {
  private final DataEntryMap<K, ?> map;

  public DataKeySet(DataEntryMap<K, ?> map) {
    this.map = map;
  }

  public int size() {
    return map.size();
  }

  public boolean isEmpty() {
    return map.size() == 0;
  }

  public boolean contains(Object key) {
    return map.find(key) != null;
  }

  public boolean add(K key) {
    throw new UnsupportedOperationException();
  }

  public boolean remove(Object key) {
    return map.remove(key) != null;
  }

  public Object[] toArray() {
    return toArray(new Object[size()]);      
  }

  public <T> T[] toArray(T[] array) {
    return Data.toArray(this, array);      
  }

  public void clear() {
    map.clear();
  }

  public Iterator<K> iterator() {
    return new DataKeyIterator(map.iterator());
  }
}
