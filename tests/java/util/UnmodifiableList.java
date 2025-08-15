/* Copyright (c) 2008-2015, Avian Contributors

   Permission to use, copy, modify, and/or distribute this software
   for any purpose with or without fee is hereby granted, provided
   that the above copyright notice and this permission notice appear
   in all copies.

   There is NO WARRANTY for this software.  See license.txt for
   details. */

package java.util;

public class UnmodifiableList<T> implements List<T> {

  private List<T> inner;

  UnmodifiableList(List<T> l) {
    this.inner = l;
  }

  public T get(int index) {
    return inner.get(index);
  }

  public T set(int index, T value) {
    throw new UnsupportedOperationException();
  }

  public T remove(int index) {
    throw new UnsupportedOperationException();
  }

  public boolean remove(Object o) {
    throw new UnsupportedOperationException();
  }

  public boolean add(T element) {
    throw new UnsupportedOperationException();
  }

  public void add(int index, T element) {
    throw new UnsupportedOperationException();
  }

  public Iterator<T> iterator() {
    return new UnmodifiableIterator<T>(inner.iterator());
  }

  public int indexOf(Object value) {
    return inner.indexOf(value);
  }

  public int lastIndexOf(Object value) {
    return inner.lastIndexOf(value);
  }

  public boolean isEmpty() {
    return inner.isEmpty();
  }

  public ListIterator<T> listIterator(int index) {
    return new UnmodifiableListIterator<T>(inner.listIterator(index));
  }

  public ListIterator<T> listIterator() {
    return new UnmodifiableListIterator<T>(inner.listIterator());
  }

  public int size() {
    return inner.size();
  }

  public boolean contains(Object element) {
    return inner.contains(element);
  }

  public boolean addAll(Collection<? extends T> collection) {
    throw new UnsupportedOperationException();
  }

  public Object[] toArray() {
    return inner.toArray();
  }

  public <S> S[] toArray(S[] array) {
    return inner.toArray(array);
  }

  public void clear() {
    throw new UnsupportedOperationException();
  }

  public boolean removeAll(Collection<?> c) {
    throw new UnsupportedOperationException();
  }

  public boolean addAll(int startIndex, Collection<? extends T> c) {
    throw new UnsupportedOperationException();
  }

  public boolean containsAll(Collection<?> c) {
    return inner.containsAll(c);
  }
}
