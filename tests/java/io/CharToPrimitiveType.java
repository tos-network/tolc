/* Copyright (c) 2008-2015, Avian Contributors

   Permission to use, copy, modify, and/or distribute this software
   for any purpose with or without fee is hereby granted, provided
   that the above copyright notice and this permission notice appear
   in all copies.

   There is NO WARRANTY for this software.  See license.txt for
   details. */

package java.io;

/**
 * ObjectInputStream1 - This class was created to handle any additional functionality
 * that might have been part of the original ObjectInputStream implementation.
 */
public class CharToPrimitiveType {
    
    private CharToPrimitiveType() {
        // Utility class, prevent instantiation
    }
    
    /**
     * Utility method for handling primitive type conversion
     */
    public static Class charToPrimitiveType(int c) {
        if (c == 'B') {
            return Byte.TYPE;
        } else if (c == 'C') {
            return Character.TYPE;
        } else if (c == 'I') {
            return Integer.TYPE;
        } else if (c == 'J') {
            return Long.TYPE;
        } else if (c == 'S') {
            return Short.TYPE;
        } else if (c == 'Z') {
            return Boolean.TYPE;
        }
        throw new RuntimeException("Unhandled char: " + (char)c);
    }
}
