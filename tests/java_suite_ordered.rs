use std::fs;
use std::path::{Path, PathBuf};
use tolc::parser::parse_and_verify;
use std::env;
use tolc::codegen::{ClassWriter, class_file_to_bytes};
use tolc::ast::TypeDecl;

fn java_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("tests").join("java")
}

fn classes_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("tests").join("classes").join("java")
}


// cargo test --test java_suite_ordered -- --nocapture
// TOLC_FILTER_FILE=ArraysComparator.java cargo test --test java_suite_ordered -- --nocapture
#[test]
fn parse_java_files_in_order() {
    // Initialize logger for diagnostics
    let _ = env_logger::builder()
        .is_test(true)
        .format_timestamp_millis()
        .filter_level(log::LevelFilter::Debug)
        .try_init();
    let root = java_root();
    assert!(root.exists(), "tests/java directory not found: {}", root.display());

    // Reference compiled classes root for javap parity
    let ref_root = classes_root();
    assert!(ref_root.exists(), "tests/classes/java not found: {}", ref_root.display());
    if env::var("TOLC_CLASSPATH").is_err() { env::set_var("TOLC_CLASSPATH", root.display().to_string()); }
    if env::var("TOLC_JAVAC_COMPAT").is_err() { env::set_var("TOLC_JAVAC_COMPAT", "1".to_string()); }

    // Check if we should filter to a specific file
    let filter_file = env::var("TOLC_FILTER_FILE").ok();

    // Predefined file list in specific order
    let file_list = vec![
        "tests/java/util/ArraysComparator.java",
        "tests/java/util/ArraysAbstractList.java",
        "tests/java/util/HashMapMyIterator.java",
        "tests/java/util/LinkedList.java",
        "tests/java/util/Map.java",
        "tests/java/util/BitSet.java",
        "tests/java/util/Objects.java",
        "tests/java/util/UnmodifiableIterator.java",
        "tests/java/util/IterationModificationException.java",
        "tests/java/util/IdentityHashMap.java",
        "tests/java/util/IdentityHashMapMyHelper.java",
        "tests/java/util/HashMap.java",
        "tests/java/util/AbstractSet.java",
        "tests/java/util/UnmodifiableList.java",
        "tests/java/util/Collection.java",
        "tests/java/util/ReverseComparator.java",
        "tests/java/util/SynchronizedList.java",
        "tests/java/util/HashMapCell.java",
        "tests/java/util/List.java",
        "tests/java/util/MaskInfo.java",
        "tests/java/util/UnmodifiableMap.java",
        "tests/java/util/HashMapHelper.java",
        "tests/java/util/ArrayListIterator.java",
        "tests/java/util/RandomAccessSynchronizedList.java",
        "tests/java/util/Queue.java",
        "tests/java/util/AbstractList.java",
        "tests/java/util/EnumSetIterator.java",
        "tests/java/util/AbstractSequentialList.java",
        "tests/java/util/SynchronizedMap.java",
        "tests/java/util/EventObject.java",
        "tests/java/util/SynchronizedIterator.java",
        "tests/java/util/LinkedListCell.java",
        "tests/java/util/SynchronizedCollection.java",
        "tests/java/util/HashMapMyEntryMap.java",
        "tests/java/util/HashMapMyHelper.java",
        "tests/java/util/ArrayList.java",
        "tests/java/util/AbstractCollection.java",
        "tests/java/util/LinkedListMyIterator.java",
        "tests/java/util/Iterator.java",
        "tests/java/util/LinkedListDescendingIterator.java",
        "tests/java/util/SynchronizedSet.java",
        "tests/java/util/ArraysListIterator.java",
        "tests/java/util/HashSetMyIterator.java",
        "tests/java/util/UnmodifiableSet.java",
        "tests/java/util/NoSuchElementException.java",
        "tests/java/util/IteratorEnumeration.java",
        "tests/java/util/Deque.java",
        "tests/java/util/Enumeration.java",
        "tests/java/util/AbstractMap.java",
        "tests/java/util/HashSet.java",
        "tests/java/util/EventListener.java",
        "tests/java/util/MaskInfoIterator.java",
        "tests/java/util/CollectionsComparator.java",
        "tests/java/util/Hashtable.java",
        "tests/java/util/UnmodifiableListIterator.java",
        "tests/java/util/IllegalFormatException.java",
        "tests/java/util/Collections.java",
        "tests/java/util/UnmodifiableCollection.java",
        "tests/java/util/EnumSet.java",
        "tests/java/util/Arrays.java",
        "tests/java/util/Comparator.java",
        "tests/java/util/Date.java",
        "tests/java/util/Set.java",
        "tests/java/util/Entry.java",
        "tests/java/util/RandomAccess.java",
        "tests/java/util/ListIterator.java",
        "tests/java/util/HashMapMyCell.java",
        "tests/java/io/PrintStream.java",
        "tests/java/io/UnsupportedEncodingException.java",
        "tests/java/io/ObjectInputStream.java",
        "tests/java/io/SystemPrintStream.java",
        "tests/java/io/ObjectOutputStream.java",
        "tests/java/io/IOException.java",
        "tests/java/io/ByteArrayOutputStreamCell.java",
        "tests/java/io/Flushable.java",
        "tests/java/io/Closeable.java",
        "tests/java/io/Serializable.java",
        "tests/java/io/InputStream.java",
        "tests/java/io/OutputStream.java",
        "tests/java/io/ObjectInputStreamClassDesc.java",
        "tests/java/io/EOFException.java",
        "tests/java/io/ByteArrayInputStream.java",
        "tests/java/io/ByteArrayOutputStream.java",
        "tests/java/io/CharToPrimitiveType.java",
        "tests/java/math/BigInteger.java",
        "tests/java/lang/Int80.java",
        "tests/java/lang/Int216.java",
        "tests/java/lang/Callable.java",
        "tests/java/lang/Int168.java",
        "tests/java/lang/Bytes1.java",
        "tests/java/lang/Int200.java",
        "tests/java/lang/Bytes12.java",
        "tests/java/lang/Int96.java",
        "tests/java/lang/IllegalStateException.java",
        "tests/java/lang/Message.java",
        "tests/java/lang/UnsatisfiedLinkError.java",
        "tests/java/lang/Int152.java",
        "tests/java/lang/System.java",
        "tests/java/lang/Bytes28.java",
        "tests/java/lang/Int144.java",
        "tests/java/lang/NullPointerException.java",
        "tests/java/lang/StringBuilder.java",
        "tests/java/lang/NumericArrays.java",
        "tests/java/lang/AutoCloseable.java",
        "tests/java/lang/Integer.java",
        "tests/java/lang/Bytes32.java",
        "tests/java/lang/contract/Contract.java",
        "tests/java/lang/contract/IERC20.java",
        "tests/java/lang/contract/ERC20.java",
        "tests/java/lang/contract/ERC20InsufficientAllowance.java",
        "tests/java/lang/contract/ERC20InvalidApprover.java",
        "tests/java/lang/contract/ERC20InvalidSpender.java",
        "tests/java/lang/contract/IERC20Metadata.java",
        "tests/java/lang/contract/ERC20TokenPaused.java",
        "tests/java/lang/contract/ERC20InvalidSender.java",
        "tests/java/lang/contract/OnlyOwner.java",
        "tests/java/lang/contract/ERC20InsufficientBalance.java",
        "tests/java/lang/contract/ERC20InvalidAmount.java",
        "tests/java/lang/contract/IERC20Errors.java",
        "tests/java/lang/contract/ERC20InvalidReceiver.java",
        "tests/java/lang/Bytes24.java",
        "tests/java/lang/NoSuchFieldException.java",
        "tests/java/lang/IllegalAccessException.java",
        "tests/java/lang/Bytes25.java",
        "tests/java/lang/AssertionError.java",
        "tests/java/lang/ThreadDeath.java",
        "tests/java/lang/Iterable.java",
        "tests/java/lang/StackTraceElement.java",
        "tests/java/lang/Uint256.java",
        "tests/java/lang/SecurityException.java",
        "tests/java/lang/VirtualMachineError.java",
        "tests/java/lang/Exception.java",
        "tests/java/lang/Int112.java",
        "tests/java/lang/StringBuffer.java",
        "tests/java/lang/Bytes29.java",
        "tests/java/lang/bytes/Type.java",
        "tests/java/lang/bytes/Hex.java",
        "tests/java/lang/bytes/Division.java",
        "tests/java/lang/Uint128.java",
        "tests/java/lang/StringBuilderCell.java",
        "tests/java/lang/Int104.java",
        "tests/java/lang/SystemNanoTime.java",
        "tests/java/lang/Int128.java",
        "tests/java/lang/RevertException.java",
        "tests/java/lang/Bytes13.java",
        "tests/java/lang/Int256.java",
        "tests/java/lang/UintType.java",
        "tests/java/lang/Int240.java",
        "tests/java/lang/Byte.java",
        "tests/java/lang/Int32.java",
        "tests/java/lang/Paused.java",
        "tests/java/lang/Int24.java",
        "tests/java/lang/StringIndexOutOfBoundsException.java",
        "tests/java/lang/Bytes18.java",
        "tests/java/lang/AbstractMethodError.java",
        "tests/java/lang/Short.java",
        "tests/java/lang/Bytes22.java",
        "tests/java/lang/OutOfMemoryError.java",
        "tests/java/lang/Bytes.java",
        "tests/java/lang/invoke/MethodHandlesLookup.java",
        "tests/java/lang/invoke/MethodTypeResult.java",
        "tests/java/lang/invoke/MethodType.java",
        "tests/java/lang/invoke/MethodHandles.java",
        "tests/java/lang/invoke/MethodTypeType.java",
        "tests/java/lang/invoke/MethodTypeTypeSpec.java",
        "tests/java/lang/invoke/MethodHandle.java",
        "tests/java/lang/invoke/MethodTypeParameter.java",
        "tests/java/lang/Void.java",
        "tests/java/lang/Context.java",
        "tests/java/lang/Int.java",
        "tests/java/lang/Bytes7.java",
        "tests/java/lang/StringUtil.java",
        "tests/java/lang/IllegalAccessError.java",
        "tests/java/lang/Bytes14.java",
        "tests/java/lang/reflect/InvocationHandler.java",
        "tests/java/lang/reflect/TypeVariable.java",
        "tests/java/lang/reflect/SignatureParser.java",
        "tests/java/lang/reflect/Method.java",
        "tests/java/lang/reflect/Type.java",
        "tests/java/lang/reflect/Modifier.java",
        "tests/java/lang/reflect/TypeVariableImpl.java",
        "tests/java/lang/reflect/Constructor.java",
        "tests/java/lang/reflect/AccessibleObject.java",
        "tests/java/lang/reflect/TypeVariableImpl1.java",
        "tests/java/lang/reflect/GenericDeclaration.java",
        "tests/java/lang/reflect/AnnotatedElement.java",
        "tests/java/lang/reflect/Array.java",
        "tests/java/lang/reflect/Proxy.java",
        "tests/java/lang/reflect/Member.java",
        "tests/java/lang/reflect/SignatureParserType.java",
        "tests/java/lang/reflect/Field.java",
        "tests/java/lang/reflect/ParameterizedType.java",
        "tests/java/lang/reflect/InvocationTargetException.java",
        "tests/java/lang/CharSequence.java",
        "tests/java/lang/TypeNotPresentException.java",
        "tests/java/lang/Deprecated.java",
        "tests/java/lang/NoSuchMethodError.java",
        "tests/java/lang/NoClassDefFoundError.java",
        "tests/java/lang/Uint8.java",
        "tests/java/lang/Package.java",
        "tests/java/lang/IllegalArgumentException.java",
        "tests/java/lang/IllegalMonitorStateException.java",
        "tests/java/lang/Comparable.java",
        "tests/java/lang/Bytes15.java",
        "tests/java/lang/Bytes6.java",
        "tests/java/lang/ClassNotFoundException.java",
        "tests/java/lang/annotation/External.java",
        "tests/java/lang/annotation/Internal.java",
        "tests/java/lang/annotation/ElementType.java",
        "tests/java/lang/annotation/RetentionPolicy.java",
        "tests/java/lang/annotation/Virtual.java",
        "tests/java/lang/annotation/Constant.java",
        "tests/java/lang/annotation/View.java",
        "tests/java/lang/annotation/Retention.java",
        "tests/java/lang/annotation/Target.java",
        "tests/java/lang/annotation/Payable.java",
        "tests/java/lang/annotation/Pure.java",
        "tests/java/lang/annotation/Annotation.java",
        "tests/java/lang/Long.java",
        "tests/java/lang/Bytes23.java",
        "tests/java/lang/Int48.java",
        "tests/java/lang/EventLog.java",
        "tests/java/lang/Class.java",
        "tests/java/lang/RuntimeException.java",
        "tests/java/lang/Uint16.java",
        "tests/java/lang/Object.java",
        "tests/java/lang/Mapping.java",
        "tests/java/lang/Int72.java",
        "tests/java/lang/Bytes19.java",
        "tests/java/lang/CloneNotSupportedException.java",
        "tests/java/lang/Throwable.java",
        "tests/java/lang/Address.java",
        "tests/java/lang/ClassCastException.java",
        "tests/java/lang/InternalError.java",
        "tests/java/lang/String.java",
        "tests/java/lang/InterruptedException.java",
        "tests/java/lang/Int64.java",
        "tests/java/lang/ReflectiveOperationException.java",
        "tests/java/lang/Uint.java",
        "tests/java/lang/Character.java",
        "tests/java/lang/Bytes20.java",
        "tests/java/lang/Int232.java",
        "tests/java/lang/Int224.java",
        "tests/java/lang/NoSuchFieldError.java",
        "tests/java/lang/Uint160.java",
        "tests/java/lang/Number.java",
        "tests/java/lang/Int208.java",
        "tests/java/lang/Int176.java",
        "tests/java/lang/Bytes9.java",
        "tests/java/lang/Approval.java",
        "tests/java/lang/Int160.java",
        "tests/java/lang/Int88.java",
        "tests/java/lang/LinkageError.java",
        "tests/java/lang/Override.java",
        "tests/java/lang/BytesType.java",
        "tests/java/lang/InstantiationException.java",
        "tests/java/lang/Cloneable.java",
        "tests/java/lang/StringComparator.java",
        "tests/java/lang/ClassLoader.java",
        "tests/java/lang/Bytes16.java",
        "tests/java/lang/Bytes5.java",
        "tests/java/lang/ExceptionInInitializerError.java",
        "tests/java/lang/IntType.java",
        "tests/java/lang/InstantiationError.java",
        "tests/java/lang/Enum.java",
        "tests/java/lang/Bytes4.java",
        "tests/java/lang/Bytes17.java",
        "tests/java/lang/NegativeArraySizeException.java",
        "tests/java/lang/Appendable.java",
        "tests/java/lang/ArrayStoreException.java",
        "tests/java/lang/Int136.java",
        "tests/java/lang/IncompatibleClassChangeError.java",
        "tests/java/lang/Int248.java",
        "tests/java/lang/Bytes8.java",
        "tests/java/lang/SuppressWarnings.java",
        "tests/java/lang/Int120.java",
        "tests/java/lang/Transfer.java",
        "tests/java/lang/Bytes21.java",
        "tests/java/lang/Int16.java",
        "tests/java/lang/ArrayIndexOutOfBoundsException.java",
        "tests/java/lang/Int8.java",
        "tests/java/lang/NumberFormatException.java",
        "tests/java/lang/Bytes10.java",
        "tests/java/lang/Uint64.java",
        "tests/java/lang/Bytes3.java",
        "tests/java/lang/Readable.java",
        "tests/java/lang/Bool.java",
        "tests/java/lang/Bytes26.java",
        "tests/java/lang/Bytes30.java",
        "tests/java/lang/ClassType.java",
        "tests/java/lang/Math.java",
        "tests/java/lang/OwnershipTransferred.java",
        "tests/java/lang/Boolean.java",
        "tests/java/lang/Error.java",
        "tests/java/lang/NumericType.java",
        "tests/java/lang/Runtime.java",
        "tests/java/lang/ArithmeticException.java",
        "tests/java/lang/NoSuchMethodException.java",
        "tests/java/lang/UnsupportedOperationException.java",
        "tests/java/lang/IndexOutOfBoundsException.java",
        "tests/java/lang/Bytes31.java",
        "tests/java/lang/Unpaused.java",
        "tests/java/lang/Bytes27.java",
        "tests/java/lang/Int184.java",
        "tests/java/lang/Int192.java",
        "tests/java/lang/Bytes2.java",
        "tests/java/lang/Bytes11.java",
        "tests/java/lang/Uint32.java",
        "tests/java/lang/SecurityManager.java",
        "tests/java/lang/Int56.java",
        "tests/java/lang/StackOverflowError.java",
        "tests/java/lang/Int40.java",
        "tests/java/base/Callable.java",
        "tests/java/base/DataEntrySet.java",
        "tests/java/base/NameAndTypePoolEntry.java",
        "tests/java/base/MethodAddendum.java",
        "tests/java/base/VMMethod.java",
        "tests/java/base/MethodRefPoolEntry.java",
        "tests/java/base/IncompatibleContinuationException.java",
        "tests/java/base/FieldData.java",
        "tests/java/base/DataKeyIterator.java",
        "tests/java/base/DataValues.java",
        "tests/java/base/ClassPoolEntry.java",
        "tests/java/base/DataKeySet.java",
        "tests/java/base/Pair.java",
        "tests/java/base/MethodData.java",
        "tests/java/base/AnnotationInvocationHandler.java",
        "tests/java/base/VMClass.java",
        "tests/java/base/ConstantPool.java",
        "tests/java/base/Utf8PoolEntry.java",
        "tests/java/base/FieldRefPoolEntry.java",
        "tests/java/base/InterfaceMethodRefPoolEntry.java",
        "tests/java/base/Singleton.java",
        "tests/java/base/IntegerPoolEntry.java",
        "tests/java/base/PoolEntry.java",
        "tests/java/base/Data.java",
        "tests/java/base/Utf8.java",
        "tests/java/base/SystemClassLoader.java",
        "tests/java/base/VMField.java",
        "tests/java/base/Stream.java",
        "tests/java/base/ClassAddendum.java",
        "tests/java/base/Callback.java",
        "tests/java/base/Assembler.java",
        "tests/java/base/InnerClassReference.java",
        "tests/java/base/StringPoolEntry.java",
        "tests/java/base/Classes.java",
        "tests/java/base/FieldAddendum.java",
        "tests/java/base/Atomic.java",
        "tests/java/base/Code.java",
        "tests/java/base/Addendum.java",
        "tests/java/base/DataValueIterator.java",
        "tests/java/base/Function.java",
        "tests/java/base/Cell.java",
        "tests/java/base/DataEntryMap.java",
    ];

    let mut failures: Vec<(String, String)> = Vec::new();
    let mut processed: usize = 0;

    // Temp dir for our generated classes
    let out_dir = std::env::temp_dir().join("java_suite_compare");
    let _ = fs::create_dir_all(&out_dir);

    eprintln!("[CONFIG] Processing {} files in predefined order", file_list.len());

    for file_path in &file_list {
        let path = Path::new(file_path);
        
        // Check if we should filter to a specific file
        if let Some(ref filter) = filter_file {
            let file_name = path.file_name().unwrap().to_string_lossy();
            if file_name != *filter {
                continue; // Skip files that don't match the filter
            }
        }
        
        // Print current file being processed
        eprintln!("[PROCESSING] {}", file_path);
        
        // Check if file exists
        if !path.exists() {
            eprintln!("[SKIP] File does not exist: {}", file_path);
            continue;
        }

        let file_name = path.file_stem().unwrap().to_string_lossy().to_string();
        
        // Read source
        let source = match fs::read_to_string(path) {
            Ok(s) => s,
            Err(e) => { 
                let error_msg = format!("IO error reading source: {}", e);
                eprintln!("[ERROR] {}: {}", file_path, error_msg);
                failures.push((file_path.to_string(), error_msg)); 
                break; // Stop on first error
            }
        };

        // Parse + review
        let ast = match parse_and_verify(&source) {
            Ok(a) => a,
            Err(e) => {
                let error_msg = format!("parse error: {}", e);
                eprintln!("[ERROR] {}: {}", file_path, error_msg);
                failures.push((file_path.to_string(), error_msg));
                break; // Stop on first error
            }
        };

        // Find matching top-level type by file name
        let type_decl_opt = ast.type_decls.iter().find(|td| match td { 
            TypeDecl::Class(c) => c.name == file_name, 
            TypeDecl::Interface(i) => i.name == file_name, 
            TypeDecl::Enum(e) => e.name == file_name, 
            TypeDecl::Annotation(a) => a.name == file_name 
        });
        let type_decl = match type_decl_opt { 
            Some(t) => t, 
            None => { 
                let error_msg = "no matching top-level type for file name".to_string();
                eprintln!("[ERROR] {}: {}", file_path, error_msg);
                failures.push((file_path.to_string(), error_msg)); 
                break; // Stop on first error
            } 
        };

        // Generate class with TOLC
        let package_name_opt = ast.package_decl.as_ref().map(|p| p.name.clone());
        let mut cw = ClassWriter::new();
        cw.set_package_name(package_name_opt.as_deref());
        cw.set_all_types(ast.type_decls.clone());
        cw.set_debug(true);
        let gen_res = match type_decl {
            TypeDecl::Class(c) => cw.generate_class(c),
            TypeDecl::Interface(i) => cw.generate_interface(i),
            TypeDecl::Enum(e) => cw.generate_enum(e),
            TypeDecl::Annotation(a) => cw.generate_annotation(a),
        };
        if let Err(e) = gen_res { 
            let error_msg = format!("codegen error: {}", e);
            eprintln!("[ERROR] {}: {}", file_path, error_msg);
            failures.push((file_path.to_string(), error_msg)); 
            break; // Stop on first error
        }
        
        let class_file = cw.get_class_file();
        let _class_bytes = class_file_to_bytes(&class_file);
        
        eprintln!("[SUCCESS] {}", file_path);
        processed += 1;
    }

    eprintln!("[SUMMARY] Total files: {}, Processed: {}, Failures: {}", file_list.len(), processed, failures.len());

    if !failures.is_empty() {
        eprintln!("First failure:");
        for (p, e) in &failures { 
            eprintln!("- {} -> {}", p, e); 
            break; // Only show first failure
        }
        panic!("Compilation failed at: {}", failures[0].0);
    } else {
        eprintln!("Success! All {} files processed successfully.", processed);
    }
}
