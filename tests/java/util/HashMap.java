/* Copyright (c) 2008-2015, Avian Contributors

   Permission to use, copy, modify, and/or distribute this software
   for any purpose with or without fee is hereby granted, provided
   that the above copyright notice and this permission notice appear
   in all copies.

   There is NO WARRANTY for this software.  See license.txt for
   details. */

package java.util;

import java.base.Data;
import java.util.NoSuchElementException;

public class HashMap<K, V> implements Map<K, V> {
  private static final int MinimumCapacity = 16;

  private int size;
  private Cell[] array;
  private final Helper helper;

  // Head and tail pointers for insertion-order linked list
  private Cell<K, V> head;
  private Cell<K, V> tail;

  public HashMap(int capacity, Helper<K, V> helper) {
    if (capacity > 0) {
      array = new Cell[Data.nextPowerOfTwo(capacity)];
    }
    this.helper = helper;
  }

  public HashMap(int capacity) {
    this(capacity, new MyHelper());
  }

  public HashMap() {
    this(0);
  }

  public HashMap(Map<K, V> map) {
    this(map.size());
    for (Map.Entry<K, V> entry : map.entrySet()) {
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
    Cell<K, V>[] newArray = null;
    if (capacity != 0) {
      capacity = Data.nextPowerOfTwo(capacity);
      if (array != null && array.length == capacity) {
        return;
      }

      newArray = new Cell[capacity];
      if (array != null) {
        for (int i = 0; i < array.length; ++i) {
          Cell<K, V> next;
          for (Cell<K, V> c = array[i]; c != null; c = next) {
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

  protected Cell<K, V> find(Object key) {
    if (array != null) {
      int index = helper.hash((K) key) & (array.length - 1);
      for (Cell<K, V> c = array[index]; c != null; c = c.next()) {
        if (helper.equal((K) key, c.getKey())) {
          return c;
        }
      }
    }
    return null;
  }

  // Insert into bucket and into insertion-order linked list
  private void insert(Cell<K, V> cell) {
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

  public void remove(Cell<K, V> cell) {
    int index = cell.hashCode() & (array.length - 1);
    Cell<K, V> p = null;
    for (Cell<K, V> c = array[index]; c != null; c = c.next()) {
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
    Cell<K, V> before = cell.before();
    Cell<K, V> after  = cell.after();
    if (before == null) head = after;
    else before.setAfter(after);
    if (after  == null) tail = before;
    else after.setBefore(before);

    -- size;
    shrink();
  }

  private Cell<K, V> putCell(K key, V value) {
    Cell<K, V> c = find(key);
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
        for (Cell<K, V> c = array[i]; c != null; c = c.next()) {
          if (helper.equal((K) value, c.getValue())) {
            return true;
          }
        }
      }
    }
    return false;
  }

  public V get(Object key) {
    Cell<K, V> c = find(key);
    return (c == null ? null : c.getValue());
  }

  public Cell<K, V> removeCell(Object key) {
    Cell<K, V> old = null;
    if (array != null) {
      int index = helper.hash((K) key) & (array.length - 1);
      Cell<K, V> p = null;
      for (Cell<K, V> c = array[index]; c != null; c = c.next()) {
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
    Cell<K, V> c = find(key);
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
    for (Map.Entry<? extends K, ? extends V> entry : elts.entrySet()) {
      put(entry.getKey(), entry.getValue());
    }
  }

  public V remove(Object key) {
    Cell<K, V> c = removeCell(key);
    return (c == null ? null : c.getValue());
  }

  public void clear() {
    array = null;
    size = 0;
    // Reset linked list
    head = tail = null;
  }

  public Set<Entry<K, V>> entrySet() {
    return new Data.EntrySet(new MyEntryMap());
  }

  public Set<K> keySet() {
    return new Data.KeySet(new MyEntryMap());
  }

  public Collection<V> values() {
    return new Data.Values(new MyEntryMap());
  }

  Iterator<Entry<K, V>> iterator() {
    return new MyIterator();
  }

  private class MyEntryMap implements Data.EntryMap<K, V> {
    public int size() {
      return HashMap.this.size();
    }

    public Entry<K,V> find(Object key) {
      return HashMap.this.find(key);
    }

    public Entry<K,V> remove(Object key) {
      return removeCell(key);
    }

    public void clear() {
      HashMap.this.clear();
    }

    public Iterator<Entry<K,V>> iterator() {
      return HashMap.this.iterator();
    }
  }

  interface Cell<K, V> extends Entry<K, V> {
    // Next pointer for hash bucket chain
    Cell<K, V> next();
    void setNext(Cell<K, V> next);

    K getKey();
    V getValue();

    // Before/after pointers for insertion-order linked list
    Cell<K, V> before();
    void setBefore(Cell<K, V> before);
    Cell<K, V> after();
    void setAfter(Cell<K, V> after);
  }

  interface Helper<K, V> {
    public Cell<K, V> make(K key, V value, Cell<K, V> next);
    
    public int hash(K key);

    public boolean equal(K a, K b);
  }

  private static class MyCell<K, V> implements Cell<K, V> {
    public final K key;
    public V value;
    public Cell<K, V> next;
    public int hashCode;
    // Pointers for insertion-order linked list
    public Cell<K, V> before, after;

    public MyCell(K key, V value, Cell<K, V> next, int hashCode) {
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

    public Cell<K, V> next() {
      return next;
    }

    public void setNext(Cell<K, V> next) {
      this.next = next;
    }

    public int hashCode() {
      return hashCode;
    }

    // Insertion-order list methods
    public Cell<K, V> before() { return before; }
    public void setBefore(Cell<K, V> b) { before = b; }
    public Cell<K, V> after() { return after; }
    public void setAfter(Cell<K, V> a) { after = a; }
  }

  static class MyHelper<K, V> implements Helper<K, V> {
    public Cell<K, V> make(K key, V value, Cell<K, V> next) {
      return new MyCell(key, value, next, hash(key));
    }

    public int hash(K a) {
      return (a == null ? 0 : a.hashCode());
    }

    public boolean equal(K a, K b) {
      return (a == null ? b == null : a.equals(b));
    }
  }

  private class MyIterator implements Iterator<Entry<K, V>> {
    private Cell<K, V> next = head;  // Start from head of linked list

    public boolean hasNext() {
      return next != null;
    }

    public Entry<K, V> next() {
      if (next == null) throw new NoSuchElementException();
      Cell<K, V> e = next;
      next = next.after();
      return e;
    }

    public void remove() {
      throw new UnsupportedOperationException("Use HashMap.remove(key) instead");
    }
  }
}
