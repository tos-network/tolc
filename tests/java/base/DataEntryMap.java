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
import java.util.Iterator;

public interface DataEntryMap<K,V> {
  public int size();

  public Entry<K,V> find(Object key);

  public Entry<K,V> remove(Object key);

  public void clear();

  public Iterator<Entry<K,V>> iterator();
}
