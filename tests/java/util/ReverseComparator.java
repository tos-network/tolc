/* Copyright (c) 2008-2015, Avian Contributors

   Permission to use, copy, modify, and/or distribute this software
   for any purpose with or without fee is hereby granted, provided
   that the above copyright notice and this permission notice appear
   in all copies.

   There is NO WARRANTY for this software.  See license.txt for
   details. */

package java.util;

public final class ReverseComparator<T> implements Comparator<T> {

  Comparator<T> cmp;
  
  public ReverseComparator(Comparator<T> cmp) {
    this.cmp = cmp;
  }
  
  public int compare(T o1, T o2) {
    return - cmp.compare(o1, o2);
  }
}
