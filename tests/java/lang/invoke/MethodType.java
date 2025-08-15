/* Copyright (c) 2008-2016, Avian Contributors

   Permission to use, copy, modify, and/or distribute this software
   for any purpose with or without fee is hereby granted, provided
   that the above copyright notice and this permission notice appear
   in all copies.

   There is NO WARRANTY for this software.  See license.txt for
   details. */

package java.lang.invoke;

import java.util.List;

import static java.base.Assembler.*;

import java.base.Assembler;
import java.base.Classes;
import java.base.SystemClassLoader;
import java.base.VMClass;
import java.util.ArrayList;

public final class MethodType implements java.io.Serializable {
  private static final char[] Primitives = new char[] {
    'V', 'Z', 'B', 'C', 'S', 'I', 'F', 'J', 'D'
  };

  final ClassLoader loader;
  final byte[] spec;
  private volatile List<MethodTypeParameter> parameters;
  private volatile MethodTypeResult result;
  private volatile int footprint;

  MethodType(ClassLoader loader, byte[] spec) {
    this.loader = loader;
    this.spec = spec;
  }

  MethodType(String spec) {
    this.loader = SystemClassLoader.appLoader();
    this.spec = new byte[spec.length() + 1];
    spec.getBytes(0, spec.length(), this.spec, 0);
  }

  public String toMethodDescriptorString() {
    return Classes.makeString(spec, 0, spec.length - 1);
  }

  private static String spec(Class c) {
    if (c.isPrimitive()) {
      VMClass vmc = Classes.toVMClass(c);
      for (char p: Primitives) {
        if (vmc == Classes.primitiveClass(p)) {
          return String.valueOf(p);
        }
      }
      throw new AssertionError();
    } else if (c.isArray()) {
      return "[" + spec(c.getComponentType());
    } else {
      return "L" + c.getName().replace('.', '/') + ";";
    }
  }

  private MethodType(Class rtype,
                     Class ... ptypes)
  {
    loader = rtype.getClassLoader();

    StringBuilder sb = new StringBuilder();
    sb.append('(');
    parameters = new ArrayList(ptypes.length);
    int position = 0;
    for (int i = 0; i < ptypes.length; ++i) {
      String spec = spec(ptypes[i]);
      sb.append(spec);

      MethodTypeType type = type(spec);

      parameters.add(new MethodTypeParameter(i,
                                   position,
                                   spec,
                                   ptypes[i],
                                   type.load));

      position += type.size;
    }
    sb.append(')');

    footprint = position;

    String spec = spec(rtype);
    sb.append(spec);

    result = new MethodTypeResult(spec, rtype, type(spec).return_);

    this.spec = sb.toString().getBytes();
  }

  public static MethodType methodType(Class rtype,
                                      Class ptype0,
                                      Class ... ptypes)
  {
    Class[] array = new Class[ptypes.length + 1];
    array[0] = ptype0;
    System.arraycopy(ptypes, 0, array, 1, ptypes.length);
    return methodType(rtype, array);
  }

  public static MethodType methodType(Class rtype,
                                      Class ... ptypes)
  {
    return new MethodType(rtype, ptypes);
  }

  public String toString() {
    return Classes.makeString(spec, 0, spec.length - 1);
  }

  public int footprint() {
    parameters(); // ensure spec is parsed

    return footprint;
  }

  public Class returnType() {
    parameters(); // ensure spec is parsed

    return result.type;
  }

  public Class[] parameterArray() {
    parameters(); // ensure spec is parsed

    Class[] array = new Class[parameters.size()];
    for (int i = 0; i < parameters.size(); ++i) {
      array[i] = parameters.get(i).type;
    }

    return array;
  }

  public Iterable<MethodTypeParameter> parameters() {
    if (parameters == null) {
      List<MethodTypeParameter> list = new ArrayList();
      int i;
      int index = 0;
      int position = 0;
      for (i = 1; spec[i] != ')'; ++i) {
        int start = i;
        switch (spec[i]) {
        case 'L': {
          ++ i;
          while (spec[i] != ';') ++ i;
        } break;

        case '[': {
          ++ i;
          while (spec[i] == '[') ++ i;

          switch (spec[i]) {
          case 'L':
            ++ i;
            while (spec[i] != ';') ++ i;
            break;

          default:
            break;
          }
        } break;

        case 'Z':
        case 'B':
        case 'S':
        case 'C':
        case 'I':
        case 'F':
        case 'J':
        case 'D':
          break;

        default: throw new AssertionError();
        }

        String paramSpec = Classes.makeString(spec, start, (i - start) + 1);
        MethodTypeType type = type(paramSpec);

        list.add(new MethodTypeParameter
                 (index,
                  position,
                  paramSpec,
                  Classes.forCanonicalName(loader, paramSpec),
                  type.load));

        ++ index;
        position += type.size;
      }

      footprint = position;

      ++ i;

      String paramSpec = Classes.makeString(spec, i, spec.length - i - 1);
              MethodTypeType type = type(paramSpec);

      result = new MethodTypeResult(paramSpec,
                          Classes.forCanonicalName(loader, paramSpec),
                          type.return_);

      parameters = list;
    }

    return parameters;
  }

  public MethodTypeResult result() {
    parameters(); // ensure spec has been parsed

    return result;
  }

  private static MethodTypeType type(String spec) {
    switch (spec.charAt(0)) {
    case 'L':
    case '[':
      return MethodTypeType.ObjectType;

    case 'Z':
    case 'B':
    case 'S':
    case 'C':
    case 'I':
      return MethodTypeType.IntegerType;

    case 'F':
      return MethodTypeType.FloatType;

    case 'J':
      return MethodTypeType.LongType;

    case 'D':
      return MethodTypeType.DoubleType;

    case 'V':
      return MethodTypeType.VoidType;

    default: throw new AssertionError();
    }
  }


}
