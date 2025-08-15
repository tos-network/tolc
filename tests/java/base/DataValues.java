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
import java.util.Collection;
import java.util.Iterator;

public class DataValues<K, V> implements Collection<V> {
  private final DataEntryMap<K, V> map;

  public DataValues(DataEntryMap<K, V> map) {
    this.map = map;
  }

  public int size() {
    return map.size();
  }

  public boolean isEmpty() {
    return map.size() == 0;
  }

  public boolean contains(Object value) {
    for (Iterator<Entry<K, V>> it = map.iterator(); it.hasNext();) {
      if (Data.equal(it.next().getValue(), value)) {
        return true;
      }
    }
    return false;
  }

  public boolean containsAll(Collection<?> c) {
    if (c == null) {
      throw new NullPointerException("collection is null");
    }
    
    for (Iterator<?> it = c.iterator(); it.hasNext();) {
      if (! contains(it.next())) {
        return false;
      }
    }
    
    return true;
  }

  public boolean add(V value) {
    throw new UnsupportedOperationException();
  }

  public boolean addAll(Collection<? extends V> collection) {
    throw new UnsupportedOperationException();      
  }

  public boolean remove(Object value) {
    for (Iterator<Entry<K, V>> it = map.iterator();
         it.hasNext();)
    {
      if (Data.equal(it.next().getValue(), value)) {
        it.remove();
        return true;
      }
    }
    return false;
  }

  public boolean removeAll(Collection<?> c) {
    boolean changed = false;
    for (Iterator<Entry<K, V>> it = map.iterator(); it.hasNext();) {
      if (c.contains(it.next().getValue())) {
        it.remove();
        changed = true;
      }
    }
    return changed;
  }

  public boolean retainAll(Collection<?> c) {
    boolean changed = false;
    for (Iterator<Entry<K, V>> it = map.iterator(); it.hasNext();) {
      if (!c.contains(it.next().getValue())) {
        it.remove();
        changed = true;
      }
    }
    return changed;
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

  public Iterator<V> iterator() {
    return new DataValueIterator(map.iterator());
  }
}
