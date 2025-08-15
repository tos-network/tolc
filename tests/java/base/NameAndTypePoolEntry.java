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

public class NameAndTypePoolEntry implements PoolEntry {
  private static final int CONSTANT_NameAndType = 12;
  private final int nameIndex;
  private final int typeIndex;

  public NameAndTypePoolEntry(int nameIndex, int typeIndex) {
    this.nameIndex = nameIndex;
    this.typeIndex = typeIndex;
  }

  public void writeTo(OutputStream out) throws IOException {
    write1(out, CONSTANT_NameAndType);
    write2(out, nameIndex + 1);
    write2(out, typeIndex + 1);
  }

  public boolean equals(Object o) {
    if (o instanceof NameAndTypePoolEntry) {
      NameAndTypePoolEntry other = (NameAndTypePoolEntry) o;
      return other.nameIndex == nameIndex && other.typeIndex == typeIndex;
    } else {
      return false;
    }
  }
}
