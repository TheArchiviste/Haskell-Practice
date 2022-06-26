// Test class that is used to visualy clarify the scoping rules of Java,
// thus being able to compare them to the one used by Haskell.
class Test {
    int x = 3;

    int method1 (int z) {
        return x + z;
    }

    public static void main(String[] args) {
        Test t = new Test();
        boolean x = true;
        
        System.out.println(t.method1(6));
    }
}