/* Copyright (c) 2008-2015, Avian Contributors

   Permission to use, copy, modify, and/or distribute this software
   for any purpose with or without fee is hereby granted, provided
   that the above copyright notice and this permission notice appear
   in all copies.

   There is NO WARRANTY for this software.  See license.txt for
   details. */

package java.util;

import java.base.Data;
import java.base.DataEntrySet;
import java.base.DataKeySet;
import java.base.DataValues;
import java.base.DataEntryMap;


import java.util.NoSuchElementException;

public class HashMap<K, V> implements Map<K, V> {
  private static final int MinimumCapacity = 16;

  private int size;
  private HashMapCell[] array;
  private final HashMapHelper helper;

  // Head and tail pointers for insertion-order linked list
  private HashMapCell<K, V> head;
  private HashMapCell<K, V> tail;

  public HashMap(int capacity, HashMapHelper<K, V> helper) {
    if (capacity > 0) {
      array = new HashMapCell[Data.nextPowerOfTwo(capacity)];
    }
    this.helper = helper;
  }

  public HashMap(int capacity) {
    this(capacity, new HashMapMyHelper());
  }

  public HashMap() {
    this(0);
  }

  // Getter for head field (needed by HashMapMyIterator)
  HashMapCell<K, V> getHead() {
    return head;
  }

  public HashMap(Map<K, V> map) {
    this(map.size());
    for (Entry<K, V> entry : map.entrySet()) {
      put(entry.getKey(), entry.getValue());
    }
  }

  public String toString() {
    return java.base.Data.toString(this);
  }

  public boolean isEmpty() {
    return size() == 0;
  }

  public int size() {
    return size;
  }

  private void grow() {
    if (array == null || size >= array.length * 2) {
      resize(array == null ? MinimumCapacity : array.length * 2);
    }
  }

  private void shrink() {
    if (array.length / 2 >= MinimumCapacity && size <= array.length / 3) {
      resize(array.length / 2);
    }
  }

  private void resize(int capacity) {
    HashMapCell<K, V>[] newArray = null;
    if (capacity != 0) {
      capacity = Data.nextPowerOfTwo(capacity);
      if (array != null && array.length == capacity) {
        return;
      }

      newArray = new HashMapCell[capacity];
      if (array != null) {
        for (int i = 0; i < array.length; ++i) {
          HashMapCell<K, V> next;
          for (HashMapCell<K, V> c = array[i]; c != null; c = next) {
            next = c.next();
            int index = c.hashCode() & (capacity - 1);
            c.setNext(newArray[index]);
            newArray[index] = c;
          }
        }
      }
    }
    array = newArray;
  }

  protected HashMapCell<K, V> find(Object key) {
    if (array != null) {
      int index = helper.hash((K) key) & (array.length - 1);
      for (HashMapCell<K, V> c = array[index]; c != null; c = c.next()) {
        if (helper.equal((K) key, c.getKey())) {
          return c;
        }
      }
    }
    return null;
  }

  // Insert into bucket and into insertion-order linked list
  private void insert(HashMapCell<K, V> cell) {
    ++ size;
    grow();

    // --- Hash bucket insertion at head ---
    int index = cell.hashCode() & (array.length - 1);
    cell.setNext(array[index]);
    array[index] = cell;

    // --- Linked list insertion at tail ---
    if (head == null) {
      head = tail = cell;
    } else {
      tail.setAfter(cell);
      cell.setBefore(tail);
      tail = cell;
    }
  }

  public void remove(HashMapCell<K, V> cell) {
    int index = cell.hashCode() & (array.length - 1);
    HashMapCell<K, V> p = null;
    for (HashMapCell<K, V> c = array[index]; c != null; c = c.next()) {
      if (c == cell) {
        if (p == null) {
          array[index] = c.next();
        } else {
          p.setNext(c.next());
        }
        break;
      }
      p = c;
    }

    // --- Remove from linked list ---
    HashMapCell<K, V> before = cell.before();
    HashMapCell<K, V> after  = cell.after();
    if (before == null) head = after;
    else before.setAfter(after);
    if (after  == null) tail = before;
    else after.setBefore(before);

    -- size;
    shrink();
  }

  private HashMapCell<K, V> putCell(K key, V value) {
    HashMapCell<K, V> c = find(key);
    if (c == null) {
      insert(helper.make(key, value, null));
    } else {
      c.setValue(value);
    }
    return c;
  }

  public boolean containsKey(Object key) {
    return find(key) != null;
  }

  public boolean containsValue(Object value) {
    if (array != null) {
      for (int i = 0; i < array.length; ++i) {
        for (HashMapCell<K, V> c = array[i]; c != null; c = c.next()) {
          if (helper.equal((K) value, c.getValue())) {
            return true;
          }
        }
      }
    }
    return false;
  }

  public V get(Object key) {
    HashMapCell<K, V> c = find(key);
    return (c == null ? null : c.getValue());
  }

  public HashMapCell<K, V> removeCell(Object key) {
    HashMapCell<K, V> old = null;
    if (array != null) {
      int index = helper.hash((K) key) & (array.length - 1);
      HashMapCell<K, V> p = null;
      for (HashMapCell<K, V> c = array[index]; c != null; c = c.next()) {
        if (helper.equal((K) key, c.getKey())) {
          old = c;
          if (p == null) array[index] = c.next();
          else p.setNext(c.next());
          break;
        }
        p = c;
      }
      if (old != null) {
        // remove from linked list
        remove(old);
      }
    }
    return old;
  }

  public V put(K key, V value) {
    HashMapCell<K, V> c = find(key);
    if (c == null) {
      insert(helper.make(key, value, null));
      return null;
    } else {
      V old = c.getValue();
      c.setValue(value);
      return old;
    }
  }

  public void putAll(Map<? extends K,? extends V> elts) {
    for (Entry<? extends K, ? extends V> entry : elts.entrySet()) {
      put(entry.getKey(), entry.getValue());
    }
  }

  public V remove(Object key) {
    HashMapCell<K, V> c = removeCell(key);
    return (c == null ? null : c.getValue());
  }

  public void clear() {
    array = null;
    size = 0;
    // Reset linked list
    head = tail = null;
  }

  public Set<Entry<K, V>> entrySet() {
    return new DataEntrySet(new HashMapMyEntryMap(this));
  }

  public Set<K> keySet() {
    return new DataKeySet(new HashMapMyEntryMap(this));
  }

  public Collection<V> values() {
    return new DataValues(new HashMapMyEntryMap(this));
  }

  Iterator<Entry<K, V>> iterator() {
    return new HashMapMyIterator(this);
  }


}
