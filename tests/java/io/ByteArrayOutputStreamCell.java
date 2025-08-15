/* Copyright (c) 2008-2015, Avian Contributors

   Permission to use, copy, modify, and/or distribute this software
   for any purpose with or without fee is hereby granted, provided
   that the above copyright notice and this permission notice appear
   in all copies.

   There is NO WARRANTY for this software.  See license.txt for
   details. */

package java.io;

public class ByteArrayOutputStreamCell {
  public byte[] array;
  public int offset;
  public int length;
  public ByteArrayOutputStreamCell next = null;

  public ByteArrayOutputStreamCell(byte[] array, int offset, int length) {
    this.array = array;
    this.offset = offset;
    this.length = length;
  }
}
