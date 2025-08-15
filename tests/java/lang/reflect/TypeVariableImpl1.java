/* Copyright (c) 2008-2015, Avian Contributors

   Permission to use, copy, modify, and/or distribute this software
   for any purpose with or without fee is hereby granted, provided
   that the above copyright notice and this permission notice appear
   in all copies.

   There is NO WARRANTY for this software.  See license.txt for
   details. */

package java.lang.reflect;

public class TypeVariableImpl1 implements GenericDeclaration {
    private final TypeVariableImpl[] vars;

    public TypeVariableImpl1(TypeVariableImpl[] vars) {
        this.vars = vars;
    }

    public TypeVariable<?>[] getTypeParameters() {
        return vars;
    }

    // Implement required AnnotatedElement methods
    public <T extends java.lang.annotation.Annotation> T getAnnotation(Class<T> annotationClass) {
        return null;
    }

    public java.lang.annotation.Annotation[] getAnnotations() {
        return new java.lang.annotation.Annotation[0];
    }

    public java.lang.annotation.Annotation[] getDeclaredAnnotations() {
        return new java.lang.annotation.Annotation[0];
    }
}
