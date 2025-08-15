/* Copyright (c) 2008-2015, Avian Contributors

   Permission to use, copy, modify, and/or distribute this software
   for any purpose with or without fee is hereby granted, provided
   that the above copyright notice and this permission notice appear
   in all copies.

   There is NO WARRANTY for this software.  See license.txt for
   details. */

package java.base;

import java.util.List;


public class ConstantPool {
  
  public static int add(List<PoolEntry> pool, PoolEntry e) {
    int i = 0;
    for (PoolEntry existing: pool) {
      if (existing.equals(e)) {
        return i;
      } else {
        ++i;
      }
    }
    pool.add(e);
    return pool.size() - 1;
  }

  public static int addInteger(List<PoolEntry> pool, int value) {
    return add(pool, new IntegerPoolEntry(value));
  }

  public static int addUtf8(List<PoolEntry> pool, String value) {
    return add(pool, new Utf8PoolEntry(value));
  }

  public static int addString(List<PoolEntry> pool, String value) {
    return add(pool, new StringPoolEntry(addUtf8(pool, value)));
  }

  public static int addClass(List<PoolEntry> pool, String name) {
    return add(pool, new ClassPoolEntry(addUtf8(pool, name)));
  }

  public static int addNameAndType(List<PoolEntry> pool,
                                   String name,
                                   String type)
  {
    return add(pool, new NameAndTypePoolEntry
               (addUtf8(pool, name),
                addUtf8(pool, type)));
  }

  public static int addFieldRef(List<PoolEntry> pool,
                                String className,
                                String name,
                                String spec)
  {
    return add(pool, new FieldRefPoolEntry
               (addClass(pool, className),
                addNameAndType(pool, name, spec)));
  }

  public static int addMethodRef(List<PoolEntry> pool,
                                 String className,
                                 String name,
                                 String spec)
  {
    return add(pool, new MethodRefPoolEntry
               (addClass(pool, className),
                addNameAndType(pool, name, spec)));
  }

  public static int addInterfaceMethodRef(List<PoolEntry> pool,
                                          String interfaceName,
                                          String name,
                                          String spec)
  {
    return add(pool, new InterfaceMethodRefPoolEntry
               (addClass(pool, interfaceName),
               addNameAndType(pool, name, spec)));
  }
}
