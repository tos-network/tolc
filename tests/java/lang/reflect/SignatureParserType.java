/* Copyright (c) 2008-2015, Avian Contributors

   Permission to use, copy, modify, and/or distribute this software
   for any purpose with or without fee is hereby granted, provided
   that the above copyright notice and this permission notice appear
   in all copies.

   There is NO WARRANTY for this software.  See license.txt for
   details. */

package java.lang.reflect;

public class SignatureParserType implements ParameterizedType {
  private final Type[] args;
  private final Type owner;
  private final Type raw;

  public SignatureParserType(Type[] args, Type owner, Type raw) {
    this.args = args;
    this.owner = owner;
    this.raw = raw;
  }

  @Override
  public Type getRawType() {
    return raw;
  }

  @Override
  public Type getOwnerType() {
    return owner;
  }

  @Override
  public Type[] getActualTypeArguments() {
    return args;
  }

  @Override
  public String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append(typeName(raw));
    builder.append('<');
    String sep = "";
    for (Type t : args) {
      builder.append(sep).append(typeName(t));
      sep = ", ";
    }
    builder.append('>');
    return builder.toString();
  }

  private static String typeName(Type type) {
    if (type instanceof Class) {
      Class<?> clazz = (Class<?>) type;
      return clazz.getName();
    }
    return type.toString();
  }
}
