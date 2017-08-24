class E {
    public static void main(String[] args) {
        System.out.println(new E1().run());
    }
}

class E1 {

    public int run() {
        int DIGITS;
        int[] a;
        int x;
        int N;
        int n;
        int d;

        DIGITS = 9000;
        N = DIGITS + 9;
        a = new int[DIGITS+9];
        x = 0;
        a[0] = 0;
        a[1] = 2;

        n = 2;
        while (n < N) {
            a[n] = 1;
            n = n + 1;
        }
        while(9 < N) {
            n = N - 1;
            while (0 < n) {
                a[n] = x - (x / n)*n;
                x = 10 * a[n-1] + x/n;
                n = n-1;
            }
            d = this.print(x);
            N = N - 1;
        }
        return 0;
    }

    public int print(int d) {
        int f;
        f = 0-1;
        while (d/f < 0-9) {
            f = f * 10;
        }
        while (f < 0) {
            System.out.print((char)(48-(d/f)));
            d = d - (d/f) * f;
            f = f/10;
        }
        return 0;
    }
}
