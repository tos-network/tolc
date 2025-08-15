/* Copyright (c) 2008-2015, Avian Contributors

   Permission to use, copy, modify, and/or distribute this software
   for any purpose with or without fee is hereby granted, provided
   that the above copyright notice and this permission notice appear
   in all copies.

   There is NO WARRANTY for this software.  See license.txt for
   details. */

package java.base;

public class FieldData {
  public final int flags;
  public final int nameIndex;
  public final int specIndex;

  public FieldData(int flags, int nameIndex, int specIndex) {
    this.flags = flags;
    this.nameIndex = nameIndex;
    this.specIndex = specIndex;
  }
}
