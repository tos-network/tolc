/* Copyright (c) 2008-2015, Avian Contributors

   Permission to use, copy, modify, and/or distribute this software
   for any purpose with or without fee is hereby granted, provided
   that the above copyright notice and this permission notice appear
   in all copies.

   There is NO WARRANTY for this software.  See license.txt for
   details. */

package java.util;

public class HashMapMyCell<K, V> implements HashMapCell<K, V> {
  public final K key;
  public V value;
  public HashMapCell<K, V> next;
  public int hashCode;
  // Pointers for insertion-order linked list
  public HashMapCell<K, V> before, after;

  public HashMapMyCell(K key, V value, HashMapCell<K, V> next, int hashCode) {
    this.key = key;
    this.value = value;
    this.next = next;
    this.hashCode = hashCode;
    // before/after default to null
  }

  public K getKey() {
    return key;
  }

  public V getValue() {
    return value;
  }

  public V setValue(V value) {
    V old = this.value;
    this.value = value;
    return old;
  }

  public HashMapCell<K, V> next() {
    return next;
  }

  public void setNext(HashMapCell<K, V> next) {
    this.next = next;
  }

  public int hashCode() {
    return hashCode;
  }

  // Insertion-order list methods
  public HashMapCell<K, V> before() { return before; }
  public void setBefore(HashMapCell<K, V> b) { before = b; }
  public HashMapCell<K, V> after() { return after; }
  public void setAfter(HashMapCell<K, V> a) { after = a; }
}
