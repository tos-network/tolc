/* Copyright (c) 2008-2015, Avian Contributors

   Permission to use, copy, modify, and/or distribute this software
   for any purpose with or without fee is hereby granted, provided
   that the above copyright notice and this permission notice appear
   in all copies.

   There is NO WARRANTY for this software.  See license.txt for
   details. */

package java.util;

public interface HashMapCell<K, V> extends Entry<K, V> {
  // Next pointer for hash bucket chain
  HashMapCell<K, V> next();
  void setNext(HashMapCell<K, V> next);

  K getKey();
  V getValue();

  // Before/after pointers for insertion-order linked list
  HashMapCell<K, V> before();
  void setBefore(HashMapCell<K, V> before);
  HashMapCell<K, V> after();
  void setAfter(HashMapCell<K, V> after);
}
