/* Copyright (c) 2008-2015, Avian Contributors

   Permission to use, copy, modify, and/or distribute this software
   for any purpose with or without fee is hereby granted, provided
   that the above copyright notice and this permission notice appear
   in all copies.

   There is NO WARRANTY for this software.  See license.txt for
   details. */

package java.util;

import java.util.NoSuchElementException;
import java.lang.IllegalStateException;

public class LinkedListMyIterator<T> implements ListIterator<T> {
  private final LinkedList<T> linkedList;
  private LinkedListCell<T> toRemove;
  private LinkedListCell<T> current;

  public LinkedListMyIterator(LinkedList<T> linkedList) {
    this.linkedList = linkedList;
  }

  public T previous() {
    if (hasPrevious()) {
      T v = current.value;
      toRemove = current;
      current = current.prev;
      return v;
    } else {
      throw new NoSuchElementException();
    }
  }

  public T next() {
    if (hasNext()) {
      if (current == null) {
        current = linkedList.getFront();
      } else {
        current = current.next;
      }
      toRemove = current;
      return current.value;
    } else {
      throw new NoSuchElementException();
    }
  }

  public boolean hasNext() {
    if (current == null) {
      return linkedList.getFront() != null;
    } else {
      return current.next != null;
    }
  }

  public boolean hasPrevious() {
    return current != null;
  }

  public void remove() {
    if (toRemove != null) {
      current = toRemove.prev;
      linkedList.removeCell(toRemove);
      toRemove = null;
    } else {
      throw new IllegalStateException();
    }
  }

  // Implement missing ListIterator methods
  public int nextIndex() {
    if (current == null) {
      return 0;
    }
    int index = 0;
    for (LinkedListCell<T> c = linkedList.getFront(); c != null && c != current.next; c = c.next) {
      index++;
    }
    return index;
  }

  public int previousIndex() {
    if (current == null) {
      return -1;
    }
    int index = 0;
    for (LinkedListCell<T> c = linkedList.getFront(); c != null && c != current; c = c.next) {
      index++;
    }
    return index - 1;
  }

  public void set(T e) {
    if (toRemove != null) {
      toRemove.value = e;
    } else {
      throw new IllegalStateException();
    }
  }

  public void add(T e) {
    if (current == null) {
      linkedList.addFirst(e);
      current = linkedList.getFront();
    } else {
      LinkedListCell<T> newCell = new LinkedListCell<>(e, current, current.next);
      if (current.next != null) {
        current.next.prev = newCell;
      }
      current.next = newCell;
      current = newCell;
      linkedList.incrementSize();
    }
    toRemove = null;
  }
}
