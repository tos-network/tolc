package java.lang.reflect;

import java.util.List;

public class TypeVariableImpl implements TypeVariable {
    private String name;
    private Type baseType;
    private TypeVariableImpl[] vars;

    public Type[] getBounds() {
      return new Type[] { baseType };
    }
    
    public GenericDeclaration getGenericDeclaration() {
      return new TypeVariableImpl1(vars);
    }
    
    public String getName() {
      return name;
    }
    
    TypeVariableImpl(String name, Type baseType) {
      this.name = name;
      this.baseType = baseType;
    }
    
    void setVars(List<TypeVariableImpl> vars) {
      this.vars = new TypeVariableImpl[vars.size()];
      vars.toArray(this.vars);
    }
    
    @Override
    public String toString() {
      return name;
    }
  }
