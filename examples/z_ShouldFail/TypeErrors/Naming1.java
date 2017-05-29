class Naming1 {
    public static void main (String[] argv) {
        System.out.println(new A().f(new A(), 3));
    }
}

class A {
    int a;

    public int f (A b, int b) {
        return a;
    }
}
