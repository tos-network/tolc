/* Copyright (c) 2008-2015, Avian Contributors

   Permission to use, copy, modify, and/or distribute this software
   for any purpose with or without fee is hereby granted, provided
   that the above copyright notice and this permission notice appear
   in all copies.

   There is NO WARRANTY for this software.  See license.txt for
   details. */

package java.base;

import java.io.OutputStream;
import java.io.IOException;

import static java.base.Stream.write1;
import static java.base.Stream.write2;

public class StringPoolEntry implements PoolEntry {
  private static final int CONSTANT_String = 8;
  private final int valueIndex;

  public StringPoolEntry(int valueIndex) {
    this.valueIndex = valueIndex;
  }

  public void writeTo(OutputStream out) throws IOException {
    write1(out, CONSTANT_String);
    write2(out, valueIndex + 1);
  }

  public boolean equals(Object o) {
    return o instanceof StringPoolEntry 
      && ((StringPoolEntry) o).valueIndex == valueIndex;
  }
}
