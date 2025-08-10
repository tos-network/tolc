/* Copyright (c) 2008-2015, Avian Contributors

   Permission to use, copy, modify, and/or distribute this software
   for any purpose with or without fee is hereby granted, provided
   that the above copyright notice and this permission notice appear
   in all copies.

   There is NO WARRANTY for this software.  See license.txt for
   details. */

package java.lang;

import java.io.InputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Enumeration;
import java.util.Map;
import java.util.HashMap;

public abstract class ClassLoader {
  private final ClassLoader parent;
  private Map<String, Package> packages;

  protected ClassLoader(ClassLoader parent) {
    if (parent == null) {
      this.parent = getSystemClassLoader();
    } else {
      this.parent = parent;
    }
  }

  protected ClassLoader() {
    this(getSystemClassLoader());
  }

  private Map<String, Package> packages() {
    if (packages == null) {
      packages = new HashMap();
    }
    return packages;
  }

  protected Package getPackage(String name) {
    Package p;
    // Simplified for blockchain VM - no synchronization needed
    p = packages().get(name);

    if (parent != null) {
      p = parent.getPackage(name);
    } else {
      // Simplified package definition without URL
      p = definePackage(name, null, null, null, null, null, null, null);
    }

    if (p != null) {
      // Simplified for blockchain VM - no synchronization needed
      Package p2 = packages().get(name);
      if (p2 != null) {
        p = p2;
      } else {
        packages().put(name, p);
      }
    }

    return p;
  }

  protected Package[] getPackages() {
    // Simplified for blockchain VM - no synchronization needed
    return packages().values().toArray(new Package[packages().size()]);
  }

  protected Package definePackage(String name,
                                  String specificationTitle,
                                  String specificationVersion,
                                  String specificationVendor,
                                  String implementationTitle,
                                  String implementationVersion,
                                  String implementationVendor,
                                  Object sealBase)
    {
      // Create Package with null URL since we removed network dependencies
      Package p = new Package
        (name, implementationTitle, implementationVersion,
         implementationVendor, specificationTitle, specificationVersion,
         specificationVendor, null, this);

      // Simplified for blockchain VM - no synchronization needed
      packages().put(name, p);
      return p;
    }

  public static ClassLoader getSystemClassLoader() {
    return java.base.SystemClassLoader.getSystemClassLoader();
  }

  protected Class defineClass(String name, byte[] b, int offset, int length) {
    // Use avian.Classes.defineVMClass and convert to Class
    java.base.VMClass vmClass = java.base.Classes.defineVMClass(this, b, offset, length);
    return new Class(vmClass);
  }

  protected Class findClass(String name) throws ClassNotFoundException {
    throw new ClassNotFoundException(name);
  }

  protected Class reallyFindLoadedClass(String name) {
    // Simplified - return null since we can't easily access avian.Classes.findLoadedClass
    return null;
  }

  protected final Class findLoadedClass(String name) {
    return reallyFindLoadedClass(name);
  }

  public Class loadClass(String name) throws ClassNotFoundException {
    return loadClass(name, false);
  }

  protected Class loadClass(String name, boolean resolve)
    throws ClassNotFoundException
  {
    Class c = findLoadedClass(name);
    if (c == null) {
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

  protected void resolveClass(Class c) {
    // Simplified - just call avian.Classes.link with the VMClass
    java.base.Classes.link(c.vmClass, this);
  }

  public final ClassLoader getParent() {
    return parent;
  }

  // Simplified resource methods without URL dependency
  protected Object findResource(String path) {
    return null;
  }

  protected Enumeration<Object> findResources(String name) throws IOException {
    return Collections.enumeration(new ArrayList<Object>(0));
  }

  public Object getResource(String path) {
    Object url = null;
    if (parent != null) {
      url = parent.getResource(path);
    }

    if (url == null) {
      url = findResource(path);
    }

    return url;
  }

  public InputStream getResourceAsStream(String path) {
    Object url = getResource(path);
    // Simplified - return null since we can't use URL.openStream()
    return null;
  }

  public static Object getSystemResource(String path) {
    return getSystemClassLoader().getResource(path);
  }

  public static InputStream getSystemResourceAsStream(String path) {
    return getSystemClassLoader().getResourceAsStream(path);
  }

  public static Enumeration<Object> getSystemResources(String name) throws IOException {
    return getSystemClassLoader().getResources(name);
  }

  public Enumeration<Object> getResources(String name)
    throws IOException {
    Collection<Object> resources = collectResources(name);
    return Collections.enumeration(resources);
  }

  private Collection<Object> collectResources(String name) {
    Collection<Object> urls = parent != null ? parent.collectResources(name) : new ArrayList<Object>(5);
    Object url = findResource(name);
    if (url != null) {
      urls.add(url);
    }
    return urls;
  }

  protected String findLibrary(String name) {
    return null;
  }

  static native Class getCaller();

  static native void load(String name, Class caller, boolean mapName);
}
