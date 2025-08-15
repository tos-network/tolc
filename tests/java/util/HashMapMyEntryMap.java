/* Copyright (c) 2008-2015, Avian Contributors

   Permission to use, copy, modify, and/or distribute this software
   for any purpose with or without fee is hereby granted, provided
   that the above copyright notice and this permission notice appear
   in all copies.

   There is NO WARRANTY for this software.  See license.txt for
   details. */

package java.util;

import java.base.DataEntryMap;
import java.util.Iterator;

public class HashMapMyEntryMap<K, V> implements DataEntryMap<K, V> {
  private final HashMap<K, V> hashMap;

  public HashMapMyEntryMap(HashMap<K, V> hashMap) {
    this.hashMap = hashMap;
  }

  public int size() {
    return hashMap.size();
  }

  public Entry<K,V> find(Object key) {
    return hashMap.find(key);
  }

  public Entry<K,V> remove(Object key) {
    return hashMap.removeCell(key);
  }

  public void clear() {
    hashMap.clear();
  }

  public Iterator<Entry<K,V>> iterator() {
    return hashMap.iterator();
  }
}
