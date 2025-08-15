/* Copyright (c) 2008-2015, Avian Contributors

   Permission to use, copy, modify, and/or distribute this software
   for any purpose with or without fee is hereby granted, provided
   that the above copyright notice and this permission notice appear
   in all copies.

   There is NO WARRANTY for this software.  See license.txt for
   details. */

package java.util;

public class SynchronizedCollection<T> implements Collection<T> {
  protected final Object lock;
  protected final Collection<T> collection;

  public SynchronizedCollection(Object lock, Collection<T> collection) {
    this.lock = lock;
    this.collection = collection;
  }

  public int size() {
    // Simplified for blockchain VM - no synchronization needed
    return collection.size();
  }

  public boolean isEmpty() {
    return size() == 0;
  }

  public boolean contains(Object e) {
    // Simplified for blockchain VM - no synchronization needed
    return collection.contains(e);
  }

  public boolean add(T e) {
    // Simplified for blockchain VM - no synchronization needed
    return collection.add(e);
  }

  public boolean addAll(Collection<? extends T> collection) {
    // Simplified for blockchain VM - no synchronization needed
    return this.collection.addAll(collection);
  }

  public boolean remove(Object e) {
    // Simplified for blockchain VM - no synchronization needed
    return collection.remove((T)e);
  }

  public Object[] toArray() {
    return toArray(new Object[size()]);      
  }

  public <T> T[] toArray(T[] array) {
    // Simplified for blockchain VM - no synchronization needed
    return collection.toArray(array);
  }

  public void clear() {
    // Simplified for blockchain VM - no synchronization needed
    collection.clear();
  }

  public Iterator<T> iterator() {
    // Simplified for blockchain VM - return direct iterator
    return collection.iterator();
  }

  public boolean containsAll(Collection<?> c) {
    // Simplified for blockchain VM - no synchronization needed
    return collection.containsAll(c);
  }

  public boolean removeAll(Collection<?> c) {
    // Simplified for blockchain VM - no synchronization needed
    return collection.removeAll(c);
  }
}
