// Should return 5

class Scope {
    public static void main (String[] argv) {
	System.out.println (new S().run(10));
    }
}

class S {

    public int run (int n) {
        int n;
        n = 5;
        return n;
    }

}