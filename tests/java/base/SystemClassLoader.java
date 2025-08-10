/* Copyright (c) 2008-2015, Avian Contributors

   Permission to use, copy, modify, and/or distribute this software
   for any purpose with or without fee is hereby granted, provided
   that the above copyright notice and this permission notice appear
   in all copies.

   There is NO WARRANTY for this software.  See license.txt for
   details. */

package java.base;

import java.io.IOException;
import java.util.Collection;
import java.util.Collections;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.NoSuchElementException;

public class SystemClassLoader extends ClassLoader {
  public static native ClassLoader appLoader();

  private native VMClass findVMClass(String name)
    throws ClassNotFoundException;

  protected Class findClass(String name) throws ClassNotFoundException {
    return getClass(findVMClass(name));
  }

  public static native Class getClass(VMClass vmClass);

  public static native VMClass vmClass(Class jClass);

  private native VMClass findLoadedVMClass(String name);

  protected Class reallyFindLoadedClass(String name){
    VMClass c = findLoadedVMClass(name);
    return c == null ? null : getClass(c);
  }

  protected Class loadClass(String name, boolean resolve)
    throws ClassNotFoundException
  {
    Class c = findLoadedClass(name);
    if (c == null) {
      ClassLoader parent = getParent();
      if (parent != null) {
        try {
          c = parent.loadClass(name);
        } catch (ClassNotFoundException ok) { }
      }

      if (c == null) {
        c = findClass(name);
      }
    }

    if (resolve) {
      resolveClass(c);
    }

    return c;
  }

  private native String resourceURLPrefix(String name);

  // Simplified resource methods without URL dependency
  protected Object findResource(String name) {
    // Simplified - return null since we removed URL functionality
    return null;
  }

  protected static native String getPackageSource(String name);

  // Simplified resource methods
  public Object getResource(String path) {
    Object url = null;
    ClassLoader parent = getParent();
    if (parent != null) {
      url = parent.getResource(path);
    }

    if (url == null) {
      url = findResource(path);
    }

    return url;
  }

  // Simplified resources method
  public Enumeration<Object> getResources(String name) throws IOException {
    Collection<Object> urls = new ArrayList<Object>(5);

    ClassLoader parent = getParent();
    if (parent != null) {
      try {
        for (Enumeration<Object> e = parent.getResources(name);
             e.hasMoreElements();)
        {
          urls.add(e.nextElement());
        }
      } catch (Exception ignored) {
        // Ignore any errors from parent
      }
    }

    Object url = findResource(name);
    if (url != null) {
      urls.add(url);
    }

    return Collections.enumeration(urls);
  }

  // Simplified resource enumeration
  protected Enumeration<Object> findResources(String name) {
    return Collections.enumeration(new ArrayList<Object>(0));
  }

  public static ClassLoader getSystemClassLoader() {
    return appLoader();
  }
}
