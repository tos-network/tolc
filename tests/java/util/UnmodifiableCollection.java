/* Copyright (c) 2008-2015, Avian Contributors

   Permission to use, copy, modify, and/or distribute this software
   for any purpose with or without fee is hereby granted, provided
   that the above copyright notice and this permission notice appear
   in all copies.

   There is NO WARRANTY for this software.  See license.txt for
   details. */

package java.util;

public class UnmodifiableCollection<T> implements Collection<T> {
  private final Collection<T> inner;
  
  UnmodifiableCollection(Collection<T> inner) {
    this.inner = inner;
  }
  
  @Override
  public Iterator<T> iterator() {
    return new UnmodifiableIterator<T>(inner.iterator());
  }

  @Override
  public int size() {
    return inner.size();
  }

  @Override
  public boolean isEmpty() {
    return inner.isEmpty();
  }

  @Override
  public boolean contains(Object element) {
    return inner.contains(element);
  }

  @Override
  public boolean containsAll(Collection<?> c) {
    return inner.containsAll(c);
  }

  @Override
  public boolean add(T element) {
    throw new UnsupportedOperationException();
  }

  @Override
  public boolean addAll(Collection<? extends T> collection) {
    throw new UnsupportedOperationException();
  }

  @Override
  public boolean remove(Object element) {
    throw new UnsupportedOperationException();
  }

  @Override
  public boolean removeAll(Collection<?> c) {
    throw new UnsupportedOperationException();
  }

  @Override
  public Object[] toArray() {
    return inner.toArray();
  }

  @Override
  public <S> S[] toArray(S[] array) {
    return inner.toArray(array);
  }

  @Override
  public void clear() {
    throw new UnsupportedOperationException();
  }
}
