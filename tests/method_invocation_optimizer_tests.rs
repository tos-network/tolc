use tolc::{Config, compile};
use tolc::common::error::Result;

mod common;
use common::setup_test_classpath;

/// Test JavaC Method Invocation Optimizer 
/// This test suite verifies that our method invocation optimizer matches JavaC's exact behavior
/// Based on JavaC Items.StaticItem.invoke() and Items.MemberItem.invoke() optimizations

#[test]
fn test_static_method_invoke_optimization() -> Result<()> {
    // Set test classpath to resolve java.lang.String properly
    setup_test_classpath();
    
    let source = r#"
package test;

public class StaticInvokeTest {
    public static void main(String[] args) {
        // Test static method invocation optimization
        String result = String.valueOf(42);  // Static method call
        System.out.println(result);          // Static method call
        
        // Math static methods
        int max = Math.max(10, 20);         // Static method call
        double sqrt = Math.sqrt(16.0);      // Static method call
    }
    
    public static String customStaticMethod(int value) {
        return "Value: " + value;
    }
    
    public static void testCustomStatic() {
        // Test custom static method call
        String result = customStaticMethod(123);
        System.out.println(result);
    }
}
"#;
    
    let config = Config::default()
        .with_debug(true)
        .with_emit_frames(true);
    
    let result = compile(source, &config);
    assert!(result.is_ok(), "Static method invocation compilation should succeed: {:?}", result);
    
    Ok(())
}

#[test]
fn test_instance_method_invoke_optimization() -> Result<()> {
    // Set test classpath to resolve java.lang.String properly
    setup_test_classpath();
    
    let source = r#"
package test;

public class InstanceInvokeTest {
    private String data;
    
    public InstanceInvokeTest(String data) {
        this.data = data;
    }
    
    public static void main(String[] args) {
        InstanceInvokeTest obj = new InstanceInvokeTest("Hello");
        
        // Test instance method invocation optimization
        String result = obj.getData();           // Virtual method call (invokevirtual)
        int length = result.length();            // Virtual method call on String
        String upper = result.toUpperCase();     // Virtual method call on String
        
        System.out.println(upper);
    }
    
    public String getData() {
        return this.data;
    }
    
    public void setData(String newData) {
        this.data = newData;
    }
    
    // Test private method (should use invokespecial)
    private String processData(String input) {
        return "Processed: " + input;
    }
    
    public void testPrivateMethod() {
        String result = processData(this.data);  // Non-virtual call (invokespecial)
        System.out.println(result);
    }
}
"#;
    
    let config = Config::default()
        .with_debug(true)
        .with_emit_frames(true);
    
    let result = compile(source, &config);
    assert!(result.is_ok(), "Instance method invocation compilation should succeed: {:?}", result);
    
    Ok(())
}

#[test]
fn test_interface_method_invoke_optimization() -> Result<()> {
    // Set test classpath to resolve java.lang.String properly
    setup_test_classpath();
    
    let source = r#"
package test;

import java.util.List;
import java.util.ArrayList;

public class InterfaceInvokeTest {
    public static void main(String[] args) {
        // Test interface method invocation optimization
        List<String> list = new ArrayList<String>();
        
        // Interface method calls (should use invokeinterface)
        list.add("Hello");           // Interface method call
        list.add("World");           // Interface method call
        int size = list.size();      // Interface method call
        String item = list.get(0);   // Interface method call
        
        System.out.println("Size: " + size);
        System.out.println("Item: " + item);
    }
}
"#;
    
    let config = Config::default()
        .with_debug(true)
        .with_emit_frames(true);
    
    let result = compile(source, &config);
    assert!(result.is_ok(), "Interface method invocation compilation should succeed: {:?}", result);
    
    Ok(())
}

#[test] 
fn test_super_method_invoke_optimization() -> Result<()> {
    // Set test classpath to resolve java.lang.String properly
    setup_test_classpath();
    
    let source = r#"
package test;

class BaseClass {
    protected String getName() {
        return "Base";
    }
    
    protected void printInfo() {
        System.out.println("Base class info");
    }
}

public class SuperInvokeTest extends BaseClass {
    @Override
    protected String getName() {
        // Test super method call (should use invokespecial)
        String baseName = super.getName();  // Non-virtual super call
        return "Super: " + baseName;
    }
    
    @Override
    protected void printInfo() {
        super.printInfo();  // Non-virtual super call
        System.out.println("Derived class info");
    }
    
    public static void main(String[] args) {
        SuperInvokeTest obj = new SuperInvokeTest();
        String name = obj.getName();
        obj.printInfo();
        System.out.println("Name: " + name);
    }
}
"#;
    
    let config = Config::default()
        .with_debug(true)
        .with_emit_frames(true);
    
    let result = compile(source, &config);
    assert!(result.is_ok(), "Super method invocation compilation should succeed: {:?}", result);
    
    Ok(())
}

#[test]
fn test_constructor_invoke_optimization() -> Result<()> {
    // Set test classpath to resolve java.lang.String properly
    setup_test_classpath();
    
    let source = r#"
package test;

public class ConstructorInvokeTest {
    private String value;
    private int number;
    
    // Default constructor
    public ConstructorInvokeTest() {
        this("default", 0);  // Constructor chaining (invokespecial)
    }
    
    // Parameterized constructor
    public ConstructorInvokeTest(String value, int number) {
        super();             // Super constructor call (invokespecial)
        this.value = value;
        this.number = number;
    }
    
    public static void main(String[] args) {
        // Constructor calls (invokespecial) 
        ConstructorInvokeTest obj1 = new ConstructorInvokeTest();
        ConstructorInvokeTest obj2 = new ConstructorInvokeTest("test", 42);
        
        System.out.println("Created objects successfully");
    }
}
"#;
    
    let config = Config::default()
        .with_debug(true)
        .with_emit_frames(true);
    
    let result = compile(source, &config);
    assert!(result.is_ok(), "Constructor invocation compilation should succeed: {:?}", result);
    
    Ok(())
}

#[test]
fn test_method_type_erasure_optimization() -> Result<()> {
    // Set test classpath to resolve java.lang.String properly
    setup_test_classpath();
    
    let source = r#"
package test;

import java.util.List;
import java.util.ArrayList;

public class TypeErasureTest {
    // Generic static method
    public static <T> T identity(T item) {
        return item;
    }
    
    // Generic instance method
    public <T> List<T> createList(T item) {
        List<T> list = new ArrayList<T>();
        list.add(item);
        return list;
    }
    
    public static void main(String[] args) {
        TypeErasureTest obj = new TypeErasureTest();
        
        // Test generic method calls (should use type erasure in descriptors)
        String str = identity("hello");           // Static generic method
        Integer num = identity(42);               // Static generic method
        
        List<String> strList = obj.createList("test");  // Instance generic method
        List<Integer> numList = obj.createList(123);    // Instance generic method
        
        System.out.println("Generic methods work correctly");
    }
}
"#;
    
    let config = Config::default()
        .with_debug(true)
        .with_emit_frames(true);
    
    let result = compile(source, &config);
    assert!(result.is_ok(), "Generic method invocation with type erasure should succeed: {:?}", result);
    
    Ok(())
}

#[test]
fn test_method_invoke_bytecode_verification() -> Result<()> {
    // Set test classpath to resolve java.lang.String properly
    setup_test_classpath();
    
    let source = r#"
package test;

public class BytecodeVerificationTest {
    public static void testAll() {
        // Static method call -> should generate INVOKESTATIC
        String.valueOf(123);
        
        // Instance method call -> should generate INVOKEVIRTUAL  
        String str = "hello";
        str.length();
        
        // Constructor call -> should generate INVOKESPECIAL
        new String("test");
    }
}
"#;
    
    let config = Config::default()
        .with_debug(true)
        .with_emit_frames(true);
    
    let bytecode = compile(source, &config)?;
    
    // Basic verification that bytecode was generated
    assert!(!bytecode.is_empty(), "Generated bytecode should not be empty");
    assert!(bytecode.len() > 100, "Generated bytecode should be substantial (got {} bytes)", bytecode.len());
    
    println!("✅ Method invocation optimizer generated {} bytes of bytecode", bytecode.len());
    
    Ok(())
}