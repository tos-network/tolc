public class SimpleConstantTest {
    public static void main(String[] args) {
        int a = 0;      // Should use iconst_0
        int b = 1;      // Should use iconst_1
        int c = 5;      // Should use iconst_5
    }
}
