/* Copyright (c) 2008-2015, Avian Contributors

   Permission to use, copy, modify, and/or distribute this software
   for any purpose with or without fee is hereby granted, provided
   that the above copyright notice and this permission notice appear
   in all copies.

   There is NO WARRANTY for this software.  See license.txt for
   details. */

package java.util;

public class EnumSet<T extends Enum<T>> extends AbstractSet<T> {
  
  private BitSet bitset;
  private Class<T> elementType;
  
  private EnumSet(int size, Class<T> type) {
    bitset = new BitSet(size);
    elementType = type;
  }
  
  @Override
  public boolean add(T element) {
    int index = element.ordinal();
    boolean contains = bitset.get(index);
    if (!contains) {
      bitset.set(index);
    }
    
    return !contains;
  }
  
  @Override
  public boolean remove(Object toRemove) {
    T element =  tryToCast(toRemove);
    
    int index = element.ordinal();
    boolean contains = bitset.get(index);
    if (contains) {
      bitset.clear(index);
    }
    
    return contains;
  }
  
  @Override
  public boolean contains(Object toCheck) {
    T element =  tryToCast(toCheck);
    int index = element.ordinal();
    return bitset.get(index);
  }
  
  @Override
  public int size() {
    return bitset.cardinality();
  }

  @Override
  public Iterator<T> iterator() {
    return new EnumSetIterator<>(this, elementType, bitset);
  }
  
  public static <T extends Enum<T>>EnumSet<T> allOf(Class<T> elementType) {
    EnumSet<T> enumSet = createEmptyEnumSet(elementType);
    enumSet.bitset.set(0, elementType.getEnumConstants().length);
    
    return enumSet;
  }
  
  public static <T extends Enum<T>>EnumSet<T> noneOf(Class<T> elementType) {    
    return createEmptyEnumSet(elementType);
  }
  
  public static <T extends Enum<T>>EnumSet<T> of(T first, T ... rest) {
    EnumSet<T> enumSet = createEmptyEnumSet(first.getDeclaringClass());
    enumSet.add(first);
    addAllElementsToSet(Arrays.asList(rest), enumSet);
    
    return enumSet;
  }
  
  public static <T extends Enum<T>>EnumSet<T> complementOf(EnumSet<T> s) {
    EnumSet<T> enumSet = copyOf(s);
    enumSet.bitset.flip(0, s.elementType.getEnumConstants().length);
    
    return enumSet;
  }
  
  public static <T extends Enum<T>>EnumSet<T> copyOf(EnumSet<T> s) {
    EnumSet<T> enumSet = createEmptyEnumSet(s.elementType);
    enumSet.bitset.or(s.bitset);
    
    return enumSet;
  }
  
  private static <T extends Enum<T>>EnumSet<T> createEmptyEnumSet(Class<T> elementType) {
    T[] constants = elementType.getEnumConstants();
    EnumSet<T> enumSet = new EnumSet<T>(constants.length, elementType);
    
    return enumSet;
  }
  
  private static <T extends Enum<T>> void addAllElementsToSet(Iterable<T> elements, EnumSet<T> enumSet) {
    for (T element : elements) {
      enumSet.add(element);
    }
  }
  
  @SuppressWarnings("unchecked")
  private T tryToCast(Object object) throws ClassCastException {
    //We want the class cast exception if we can't convert.
    return (T) object;
  }


}
