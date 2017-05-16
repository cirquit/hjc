class Bounds1 {
    public static void main (String[] argv) {
        System.out.println(new A().f());
    }
}

class A {

    public int f () {
	int[] a;
	a = new int[1];	
        return a[0-1];
    }
}
