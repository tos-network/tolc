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

public class Utf8PoolEntry implements PoolEntry {
  private static final int CONSTANT_Utf8 = 1;
  private final String data;

  public Utf8PoolEntry(String data) {
    this.data = data;
  }

  public void writeTo(OutputStream out) throws IOException {
    write1(out, CONSTANT_Utf8);
    byte[] bytes = data.getBytes();
    write2(out, bytes.length);
    out.write(bytes);
  }

  public boolean equals(Object o) {
    return o instanceof Utf8PoolEntry
      && ((Utf8PoolEntry) o).data.equals(data);
  }
}
