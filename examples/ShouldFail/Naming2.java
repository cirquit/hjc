class Naming2 {
    public static void main (String[] argv) {
        System.out.println(new A().f(new A(), 3));
    }
}

class A {
    int a;

    public int f (A a, int b) {
        return a;
    }
}
