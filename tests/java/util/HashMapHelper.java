/* Copyright (c) 2008-2015, Avian Contributors

   Permission to use, copy, modify, and/or distribute this software
   for any purpose with or without fee is hereby granted, provided
   that the above copyright notice and this permission notice appear
   in all copies.

   There is NO WARRANTY for this software.  See license.txt for
   details. */

package java.util;

public interface HashMapHelper<K, V> {
  public HashMapCell<K, V> make(K key, V value, HashMapCell<K, V> next);
  
  public int hash(K key);

  public boolean equal(K a, K b);
}
