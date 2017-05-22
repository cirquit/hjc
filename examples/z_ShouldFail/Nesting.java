class Nesting {
    public static void main (String[] argv) {
        System.out.println(new A().f());
    }
}

class A {
    int a;

    public int f () {
        return this.g(this.g(this.f(), a), 3).f();
    }

    public A g (A b, int k) {
        return b;
    }
}
