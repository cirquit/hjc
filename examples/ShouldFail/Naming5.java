class Naming5 {
    public static void main (String[] argv) {
        System.out.println(new A().f(new A(), 3));
    }
}

class A {
    int a;

    public int f (A b, int c) {
        return a;
    }

    public bool f(A b, int c) {
	return true;
    }
}
