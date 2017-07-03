// Should throw exception

class ArrayBounds {
    public static void main (String[] argv) {
	System.out.println (new A().run(5, 10));
    }
}

class A {

    int[] arr;
    int[] arr2;

    public int run(int n, int m) {
        int k;
        arr = new int[m];
        k = 0;
        while  (k < m+1) {
            // Harmful in case k=m: out of bounds
            arr[k] = 2*k;
            k = k + 1;
        }
        arr2 = new int[m];
        k = 0;
        while  (k < m+1) {
            // Harmful in case k=m: out of bounds
            arr2[k] = 2*k+1;
            k = k + 1;
        }
        return arr[m];
    }

}