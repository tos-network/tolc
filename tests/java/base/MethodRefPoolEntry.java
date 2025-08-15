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

public class MethodRefPoolEntry implements PoolEntry {
  private static final int CONSTANT_Methodref = 10;
  private final int classIndex;
  private final int nameAndTypeIndex;

  public MethodRefPoolEntry(int classIndex, int nameAndTypeIndex) {
    this.classIndex = classIndex;
    this.nameAndTypeIndex = nameAndTypeIndex;
  }

  public void writeTo(OutputStream out) throws IOException {
    write1(out, CONSTANT_Methodref);
    write2(out, classIndex + 1);
    write2(out, nameAndTypeIndex + 1);
  }

  public boolean equals(Object o) {
    if (o instanceof MethodRefPoolEntry) {
      MethodRefPoolEntry other = (MethodRefPoolEntry) o;
      return other.classIndex == classIndex
        && other.nameAndTypeIndex == nameAndTypeIndex;
    } else {
      return false;
    }
  }
}
