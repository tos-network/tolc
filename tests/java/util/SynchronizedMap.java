/* Copyright (c) 2008-2015, Avian Contributors

   Permission to use, copy, modify, and/or distribute this software
   for any purpose with or without fee is hereby granted, provided
   that the above copyright notice and this permission notice appear
   in all copies.

   There is NO WARRANTY for this software.  See license.txt for
   details. */

package java.util;

public class SynchronizedMap<K,V> implements Map<K,V> {
  protected final Object lock;
  protected final Map<K,V> map;

  SynchronizedMap(Map<K,V> map) {
    this.map = map;
    this.lock = this;
  }

  SynchronizedMap(Object lock, Map<K,V> map) {
    this.map = map;
    this.lock = lock;
  }
  
  public void clear() {
    // Simplified for blockchain VM - no synchronization needed
    map.clear();
  }
  public boolean containsKey(Object key) {
    // Simplified for blockchain VM - no synchronization needed
    return map.containsKey(key);
  }
  public boolean containsValue(Object value) {
    // Simplified for blockchain VM - no synchronization needed
    return map.containsValue(value);
  }
  public Set<java.util.Entry<K, V>> entrySet() {
    // Simplified for blockchain VM - return direct set
    return map.entrySet();
  }
  public V get(Object key) {
    // Simplified for blockchain VM - no synchronization needed
    return map.get(key);
  }
  public boolean isEmpty() {
    // Simplified for blockchain VM - no synchronization needed
    return map.isEmpty();
  }
  public Set<K> keySet() {
    // Simplified for blockchain VM - return direct set
    return map.keySet();
  }
  public V put(K key, V value) {
    // Simplified for blockchain VM - no synchronization needed
    return map.put(key, value);
  }
  public void putAll(Map<? extends K, ? extends V> elts) {
    // Simplified for blockchain VM - no synchronization needed
    map.putAll(elts);
  }
  public V remove(Object key) {
    // Simplified for blockchain VM - no synchronization needed
    return map.remove(key);
  }
  public int size() {
    // Simplified for blockchain VM - no synchronization needed
    return map.size();
  }
  public Collection<V> values() {
    // Simplified for blockchain VM - return direct collection
    return map.values();
  }
}
