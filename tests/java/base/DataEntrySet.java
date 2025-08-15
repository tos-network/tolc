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
import java.util.AbstractSet;
import java.util.Iterator;

public class DataEntrySet<K, V> extends AbstractSet<Entry<K, V>> {
  private final DataEntryMap<K, V> map;

  public DataEntrySet(DataEntryMap<K, V> map) {
    this.map = map;
  }

  public int size() {
    return map.size();
  }

  public boolean isEmpty() {
    return map.size() == 0;
  }

  public boolean contains(Object o) {
    return (o instanceof Entry<?,?>)
      && map.find(((Entry<?,?>)o).getKey()) != null;
  }

  public boolean add(Entry<K, V> e) {
    throw new UnsupportedOperationException();
  }

  public boolean remove(Object o) {
    return (o instanceof Entry<?,?>)
      && map.remove(((Entry<?,?>) o).getKey()) != null;
  }

  public boolean remove(Entry<K, V> e) {
    return map.remove(e.getKey()) != null;
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

  public Iterator<Entry<K, V>> iterator() {
    return map.iterator();
  }
}
