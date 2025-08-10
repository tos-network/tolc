/* Copyright (c) 2008-2015, Avian Contributors

   Permission to use, copy, modify, and/or distribute this software
   for any purpose with or without fee is hereby granted, provided
   that the above copyright notice and this permission notice appear
   in all copies.

   There is NO WARRANTY for this software.  See license.txt for
   details. */

package java.lang;

import java.io.SystemPrintStream;

public abstract class System {
  private static class NanoTime {
    public static final long BaseInMillis = currentTimeMillis();
  }
  
  public static final SystemPrintStream out;
  public static final SystemPrintStream err;
  public static final SystemPrintStream in;

  static {
    out = new SystemPrintStream();
    err = new SystemPrintStream();
    in = null;
  }

  public static native void arraycopy(Object src, int srcOffset, Object dst,
                                      int dstOffset, int length);

  public static String getProperty(String name) {
    // Return default values for critical system properties
    if ("line.separator".equals(name)) {
      return "\n";
    } else if ("file.separator".equals(name)) {
      return "/";
    } else if ("os.name".equals(name)) {
      return "Unix";
    } else if ("path.separator".equals(name)) {
      return ":";
    } else if ("java.io.tmpdir".equals(name)) {
      return "/tmp";
    } else if ("user.language".equals(name)) {
      return "en";
    } else if ("user.region".equals(name)) {
      return "US";
    }
    
    return "";
  }
  
  public static String getProperty(String name, String defaultValue) {
    String result = getProperty(name);
    if (result.isEmpty()) {
      return defaultValue;
    }
    return result;
  }
  

  public static native long currentTimeMillis();

  public static native int identityHashCode(Object o);

  public static long nanoTime() {
    return (currentTimeMillis() - NanoTime.BaseInMillis) * 1000000;
  }


  public static void load(String path) {
  }

  public static void loadLibrary(String name) {
  }

  public static void gc() {
  }

  public static void exit(int code) {
  }
  
  public static native void setOut0(SystemPrintStream out);
  public static native void setErr0(SystemPrintStream err);
  public static native void setIn0(SystemPrintStream in);
  public static native void initSystemOut();
}
