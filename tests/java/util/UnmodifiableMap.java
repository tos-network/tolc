/* Copyright (c) 2008-2015, Avian Contributors

   Permission to use, copy, modify, and/or distribute this software
   for any purpose with or without fee is hereby granted, provided
   that the above copyright notice and this permission notice appear
   in all copies.

   There is NO WARRANTY for this software.  See license.txt for
   details. */

package java.util;

public class UnmodifiableMap<K, V> implements Map<K, V> {
  private Map<K, V> inner;

  UnmodifiableMap(Map<K, V> m) {
    this.inner = m;
  }

  public void clear() {
    throw new UnsupportedOperationException();
  }

  public boolean containsKey(Object key) {
    return inner.containsKey(key);
  }

  public boolean containsValue(Object value) {
    return inner.containsValue(value);
  }

  public Set<Entry<K, V>> entrySet() {
    return Collections.unmodifiableSet(inner.entrySet());
  }

  public V get(Object key) {
    return inner.get(key);
  }

  public boolean isEmpty() {
    return inner.isEmpty();
  }

  public Set<K> keySet() {
    return Collections.unmodifiableSet(inner.keySet());
  }

  public V put(K key, V value) {
    throw new UnsupportedOperationException();
  }

  public void putAll(Map<? extends K, ? extends V> t) {
    throw new UnsupportedOperationException();
  }

  public V remove(Object key) {
    throw new UnsupportedOperationException();
  }

  public int size() {
    return inner.size();
  }

  public Collection<V> values() {
    return Collections.unmodifiableCollection(inner.values());
  }
}
