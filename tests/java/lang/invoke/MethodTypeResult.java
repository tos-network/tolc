/* Copyright (c) 2008-2016, Avian Contributors

   Permission to use, copy, modify, and/or distribute this software
   for any purpose with or without fee is hereby granted, provided
   that the above copyright notice and this permission notice appear
   in all copies.

   There is NO WARRANTY for this software.  See license.txt for
   details. */

package java.lang.invoke;

public class MethodTypeResult implements MethodTypeTypeSpec {
  private final String spec;
  public final Class type;
  private final int return_;

  public MethodTypeResult(String spec, Class type, int return_) {
    this.spec = spec;
    this.type = type;
    this.return_ = return_;
  }

  public int return_() {
    return return_; // :)
  }

  public String spec() {
    return spec;
  }

  public Class type() {
    return type;
  }
}
