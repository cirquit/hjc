class Nesting {
    public static void main (String[] argv) {
        System.out.println(new B().f());
    }
}

class A {
}

class B {
    public int f () {
        return new A();
    }
}
