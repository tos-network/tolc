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
import static java.base.Stream.write4;

public class IntegerPoolEntry implements PoolEntry {
  private static final int CONSTANT_Integer = 3;
  private final int value;

  public IntegerPoolEntry(int value) {
    this.value = value;
  }

  public void writeTo(OutputStream out) throws IOException {
    write1(out, CONSTANT_Integer);
    write4(out, value);
  }

  public boolean equals(Object o) {
    return o instanceof IntegerPoolEntry 
      && ((IntegerPoolEntry) o).value == value;
  }
}
