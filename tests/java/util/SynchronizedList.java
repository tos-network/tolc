/* Copyright (c) 2008-2015, Avian Contributors

   Permission to use, copy, modify, and/or distribute this software
   for any purpose with or without fee is hereby granted, provided
   that the above copyright notice and this permission notice appear
   in all copies.

   There is NO WARRANTY for this software.  See license.txt for
   details. */

package java.util;

public class SynchronizedList<T>
  extends SynchronizedCollection<T>
  implements List<T>
{
  private final List<T> list;
  
  public SynchronizedList(List<T> list) {
    super(list, list);
    
    this.list = list;
  }

  @Override
  public T get(int index) {
    // Simplified for blockchain VM - no synchronization needed
    return list.get(index);
  }

  @Override
  public T set(int index, T value) {
    // Simplified for blockchain VM - no synchronization needed
    return list.set(index, value);
  }

  @Override
  public T remove(int index) {
    // Simplified for blockchain VM - no synchronization needed
    return list.remove(index);
  }

  @Override
  public void add(int index, T element) {
    // Simplified for blockchain VM - no synchronization needed
    list.add(index, element);
  }

  @Override
  public boolean addAll(int startIndex, Collection<? extends T> c) {
    // Simplified for blockchain VM - no synchronization needed
    return list.addAll(startIndex, c);
  }

  @Override
  public int indexOf(Object value) {
    // Simplified for blockchain VM - no synchronization needed
    return list.indexOf(value);
  }

  @Override
  public int lastIndexOf(Object value) {
    // Simplified for blockchain VM - no synchronization needed
    return list.lastIndexOf(value);
  }

  @Override
  public ListIterator<T> listIterator(int index) {
    // as described in the javadocs, user should be synchronized on list before calling
    return list.listIterator(index);
  }

  @Override
  public ListIterator<T> listIterator() {
    // as described in the javadocs, user should be synchronized on list before calling
    return list.listIterator();
  }
}
