/* Copyright (c) 2008-2015, Avian Contributors

   Permission to use, copy, modify, and/or distribute this software
   for any purpose with or without fee is hereby granted, provided
   that the above copyright notice and this permission notice appear
   in all copies.

   There is NO WARRANTY for this software.  See license.txt for
   details. */

package java.util;

public class HashMapMyHelper<K, V> implements HashMapHelper<K, V> {
  public HashMapCell<K, V> make(K key, V value, HashMapCell<K, V> next) {
    return new HashMapMyCell(key, value, next, hash(key));
  }

  public int hash(K a) {
    return (a == null ? 0 : a.hashCode());
  }

  public boolean equal(K a, K b) {
    return (a == null ? b == null : a.equals(b));
  }
}
