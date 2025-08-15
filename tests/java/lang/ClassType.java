/* Copyright (c) 2008-2015, Avian Contributors

   Permission to use, copy, modify, and/or distribute this software
   for any purpose with or without fee is hereby granted, provided
   that the above copyright notice and this permission notice appear
   in all copies.

   There is NO WARRANTY for this software.  See license.txt for
   details. */

package java.lang;

/**
 * Enum representing the different types of classes.
 * 
 * There are four class types: global (no dollar sign), anonymous (only digits after the dollar sign),
 * local (starts with digits after the dollar, ends in class name) and member (does not start with digits
 * after the dollar sign).
 */
public enum ClassType { 
  GLOBAL, 
  MEMBER, 
  LOCAL, 
  ANONYMOUS 
}
