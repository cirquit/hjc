class Naming3 {
    public static void main (String[] argv) {
        System.out.println(new A().f(new A(), 3));
    }
}

class A {

    public int f (A b, int a) {
	int a;
        return 2;
    }
}
