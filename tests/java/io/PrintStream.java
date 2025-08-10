package java.io;

public interface PrintStream {
    // print
    void print(String s);
    void print(Object obj);
    void print(boolean b);
    void print(char c);
    void print(int i);
    void print(long l);
    void print(char[] s);

    // println
    void println();
    void println(String x);
    void println(Object x);
    void println(boolean x);
    void println(char x);
    void println(int x);
    void println(long x);
    void println(char[] x);

    void flush();
}
