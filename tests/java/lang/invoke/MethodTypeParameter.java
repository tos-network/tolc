/* Copyright (c) 2008-2016, Avian Contributors

   Permission to use, copy, modify, and/or distribute this software
   for any purpose with or without fee is hereby granted, provided
   that the above copyright notice and this permission notice appear
   in all copies.

   There is NO WARRANTY for this software.  See license.txt for
   details. */

package java.lang.invoke;

public class MethodTypeParameter implements MethodTypeTypeSpec {
  private final int index;
  private final int position;
  private final String spec;
  public final Class type;
  private final int load;

  public MethodTypeParameter(int index,
                    int position,
                    String spec,
                    Class type,
                    int load)
  {
    this.index = index;
    this.position = position;
    this.spec = spec;
    this.type = type;
    this.load = load;
  }

  public int index() {
    return index;
  }

  public int position() {
    return position;
  }

  public String spec() {
    return spec;
  }

  public Class type() {
    return type;
  }

  public int load() {
    return load;
  }
}
