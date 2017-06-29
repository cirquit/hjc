// Should return 5

class Precedence {
    public static void main (String[] argv) {
	System.out.println (new B().run(5, 10));
    }
}

class B {

    public int run(int n, int m) {
        int k;
        if (0 < 1 && n + m * m < (n + m) * m)
            k = n;
        else k = m;
        return k;
    }

}