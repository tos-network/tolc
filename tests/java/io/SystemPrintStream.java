/* Copyright (c) 2008-2015, Avian Contributors

   Permission to use, copy, modify, and/or distribute this software
   for any purpose with or without fee is hereby granted, provided
   that the above copyright notice and this permission notice appear
   in all copies.

   There is NO WARRANTY for this software.  See license.txt for
   details. */

package java.io;

public class SystemPrintStream implements PrintStream {

  public SystemPrintStream() {
  }


 public void print(boolean b) {
      nativePrint(b ? 1 : 0);
  }


  public void print(char c) {
      nativePrint(c);
  }

  public void print(int i) {
      nativePrint(i);
  }

  public void print(long l) {
      nativePrint(l);
  }

  public void print(char s[]) {
      nativePrint(s);
  }


  public void print(String s) {
      if (s == null) {
          s = "";
      }
      nativePrint(s);
  }


  public void print(Object obj) {
    nativePrint(obj);
  }



  public void println() {
    nativePrint("\n");
  }


  public void println(boolean x) {
    nativePrint(x);
  }


  public void println(char x) {
    nativePrint(x);
  }

  public void println(int x) {
    nativePrint(x);
  }

  public void println(long x) {
    nativePrint(x);
  }

  public void println(char x[]) {
    nativePrint(x);
  }


  public void println(String x) {
      if (x == null) {
          x = "";
      }
      nativePrint(x);
  }


  public void println(Object x) {
    nativePrint(x);
  }

  public void flush() {
    // do nothing
  }

  private static native void nativePrint(long value);

  private static native void nativePrint(char[] value);

  private static native void nativePrint(Object value);

  private static native void nativePrint(boolean value);

  private static native void nativePrint(int value);

  private static native void nativePrint(String value);


}
