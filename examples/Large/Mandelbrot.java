
class Mandelbrot {
  public static void main(String[] args) {
	 System.out.println(new Mandel().m());
  }
}

class Mandel {

  int z0r;
  int z0i;
  int d;
  int[] powers;

  public int init() {
	 z0r = 0-600;
	 z0i = 0;
	 d = 7;
	 powers = new int[64];
	 return 0;
  }

  public int m() {

	 int cr;
	 int ci;
	 int zr;
	 int zi;
	 int n;
	 int x;
	 int y;
	 int t;
	 int absz;

	 x = this.init();
    System.out.print((char)80);
	 System.out.println(3);
	 System.out.println(400);
	 System.out.println(400);
	 System.out.println(60);

	 y = 0;
	 while (y < 400) {
		 ci  = z0i + (y - 200) * d;
		 x = 0;
		 while (x < 400) {
			cr  = z0r + (x - 200) * d;
			zr = 0;
			zi = 0;
			absz = 0;
			n = 0;
			while (absz < 4194304 && n < 60) {
			  t = zr;
			  zr = this.shr10(zr * zr) - this.shr10(zi * zi) + cr;
			  zi = 2*this.shr10(t * zi) + ci;
			  absz = zr * zr + zi * zi;
			  n = n+1;
			}
			n = this.printdot(n);
			x = x + 1;
		 }
		 y = y + 1;
	 }
	 return 0;
  }

  public int printdot(int v) {
	 int h;
	 int p;
	 int q;
	 int t;
	 int i;
    h = this.div(6*v*1024, 60);
    i = this.div(6*v, 60);
	 t = h - i*1024;
	 q = 1024 - t;
	 if (59 < v) {
		System.out.println(0);
		System.out.println(0);
		System.out.println(0);
	 } else if (i < 1) {
		System.out.println(60);
		System.out.println(this.shr10(60*t));
		System.out.println(0);
	 } else if (i < 2) {
		System.out.println(this.shr10(60*q));
		System.out.println(60);
		System.out.println(0);
	 } else if (i < 3) {
		System.out.println(0);
		System.out.println(60);
		System.out.println(this.shr10(60*t));
	 } else if (i < 4) {
		System.out.println(0);
		System.out.println(this.shr10(60*q));
		System.out.println(60);
	 } else if (i < 5) {
		System.out.println(this.shr10(60*t));
		System.out.println(0);
		System.out.println(60);
	 } else {
		System.out.println(60);
		System.out.println(0);
		System.out.println(this.shr10(60*q));
	 }
	 return 0;
  }

  // binaere Division
  public int div(int dividend, int dividor) {
	 boolean pos;
    int j;
    int k;
    int b;
    int res;

	 // Wir rechnen mit negativen Zahlen, da es mehr negative int-Werte gibt
	 // als positive.
	 pos = (0 < dividend);
	 if (pos) {
		dividend = 0 - dividend;
	 } else {}

	 if (0 < dividor) {
		dividor = 0 - dividor;
	 } else {
		pos = !pos;
	 }

	 powers[0] = 1;
    k = 0;
	 b = 1;
	 while (dividend < dividor * powers[k]) { 
		k = k + 1;
		b = 2 * b;
		powers[k] = b;
	 }

    res = 0;
	 while (0-1 < k && dividend < 0) {
		if (dividend < powers[k] * dividor + 1) {
		  dividend = dividend - powers[k] * dividor;
		  res = res + powers[k];
		} else {}
		k = k - 1;
	 }

	 if (!pos) {
		res = 0 - res;
	 } else {}
    return res;
  }

  public int shr10(int v) {
	 return this.div(v, 1024);
  }
}
