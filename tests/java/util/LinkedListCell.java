/* Copyright (c) 2008-2015, Avian Contributors

   Permission to use, copy, modify, and/or distribute this software
   for any purpose with or without fee is hereby granted, provided
   that the above copyright notice and this permission notice appear
   in all copies.

   There is NO WARRANTY for this software.  See license.txt for
   details. */

package java.util;

public class LinkedListCell<T> {
  public T value;
  public LinkedListCell<T> prev;
  public LinkedListCell<T> next;

  public LinkedListCell(T value, LinkedListCell<T> prev, LinkedListCell<T> next) {
    this.value = value;
    this.prev = prev;
    this.next = next;
  }
}
