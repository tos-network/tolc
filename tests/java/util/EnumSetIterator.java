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

public class EnumSetIterator<T extends Enum<T>> implements Iterator<T> {
  private final EnumSet<T> enumSet;
  private final Class<T> elementType;
  private final BitSet bitset;
  private int currentIndex = 0;
  private boolean removeAllowed = false;
  
  public EnumSetIterator(EnumSet<T> enumSet, Class<T> elementType, BitSet bitset) {
    this.enumSet = enumSet;
    this.elementType = elementType;
    this.bitset = bitset;
  }
  
  public T next() {
    if (!hasNext()) {
      throw new NoSuchElementException("EnumSet has no more elements");
    }

    int indexOfNextValue = nextIndex();
    T element = elementType.getEnumConstants()[indexOfNextValue];
    currentIndex = indexOfNextValue + 1;
    removeAllowed = true;
    
    return element;
  }

  public boolean hasNext() {
    int indexOfNextValue = nextIndex();
    if (indexOfNextValue >= 0) {
      return true;
    } else {
      return false;
    }
  }

  public void remove() {
    if (!removeAllowed) {
      throw new IllegalStateException("Cannot remove from this iterator in this state");
    }
    
    bitset.clear(currentIndex - 1);
    removeAllowed = false;
  }
  
  private int nextIndex() {
    return bitset.nextSetBit(currentIndex);
  }
}
