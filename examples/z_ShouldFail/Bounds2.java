class Bounds2 {
    public static void main (String[] argv) {
        System.out.println(new A().f(1));
    }
}

class A {

    public int f (int i) {
	int[] a;
	a = new int[1];	
        return a[i];
    }
}
