/* Copyright (c) 2008-2015, Avian Contributors

   Permission to use, copy, modify, and/or distribute this software
   for any purpose with or without fee is hereby granted, provided
   that the above copyright notice and this permission notice appear
   in all copies.

   There is NO WARRANTY for this software.  See license.txt for
   details. */

package java.util;

public class Hashtable<K, V> implements Map<K, V> {
  private final HashMap<K, V> map;

  public Hashtable(int capacity) {
    map = new HashMap(capacity);
  }

  public Hashtable() {
    this(0);
  }

  public Hashtable(Map<? extends K,? extends V> m) {
    this(m.size());
    for (Entry<? extends K, ? extends V> entry : m.entrySet()) {
      put(entry.getKey(), entry.getValue());
    }
  }

  public String toString() {
    return map.toString();
  }

  public boolean isEmpty() {
    return map.isEmpty();
  }

  public int size() {
    return map.size();
  }

  public boolean containsKey(Object key) {
    return map.containsKey(key);
  }

  public boolean containsValue(Object value) {
    return map.containsValue(value);
  }

  public V get(Object key) {
    return map.get(key);
  }

  public V put(K key, V value) {
    return map.put(key, value);
  }

  public void putAll(Map<? extends K,? extends V> elts) {
    map.putAll(elts);
  }

  public V remove(Object key) {
    return map.remove(key);
  }

  public void clear() {
    map.clear();
  }

  public Enumeration<K> keys() {
    // Simplified for blockchain VM - return null since Enumeration is not needed
    return null;
  }

  public Enumeration<V> elements() {
    // Simplified for blockchain VM - return null since Enumeration is not needed
    return null;
  }

  public Set<Entry<K, V>> entrySet() {
    // Simplified for blockchain VM - return direct set
    return map.entrySet();
  }

  public Set<K> keySet() {
    // Simplified for blockchain VM - return direct set
    return map.keySet();
  }

  public Collection<V> values() {
    // Simplified for blockchain VM - return direct collection
    return map.values();
  }

}
