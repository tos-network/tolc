/* Copyright (c) 2008-2016, Avian Contributors

   Permission to use, copy, modify, and/or distribute this software
   for any purpose with or without fee is hereby granted, provided
   that the above copyright notice and this permission notice appear
   in all copies.

   There is NO WARRANTY for this software.  See license.txt for
   details. */

package java.lang.invoke;

import static java.base.Assembler.*;
import java.base.Assembler;

public enum MethodTypeType {
  ObjectType(aload, areturn, 1),
  IntegerType(iload, ireturn, 1),
  FloatType(fload, freturn, 1),
  LongType(lload, lreturn, 2),
  DoubleType(dload, dreturn, 2),
  VoidType(-1, Assembler.return_, -1);

  public final int load;
  public final int return_;
  public final int size;

  private MethodTypeType(int load, int return_, int size) {
    this.load = load;
    this.return_ = return_;
    this.size = size;
  }
}
