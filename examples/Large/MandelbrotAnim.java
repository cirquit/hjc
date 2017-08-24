
class MandelbrotAnim {
  public static void main(String[] args) {
	 System.out.print((char)new Mandel().m());
  }
}

class Mandel {

  int col;
  int z0r;
  int z0i;
  int d;
  int maxcol;
  int maxit;

  public int init() {
	 z0r = 0-600;
	 z0i = 0;
	 d = 50;
    maxcol = 255;
    maxit = 2;
          System.out.print ((char)27);
          System.out.print ((char)91);
          System.out.print ((char)63);
          System.out.print ((char)55);
          System.out.print ((char)108);
	 return 0;
  }

  public int m() {
    int z;
	 z = this.init();
    while (maxit < 100) {
       z = this.cursorToOrigin();
       z = this.n();
       maxit = maxit + 1;
      z = this.delay();
      z = this.delay();
    }
    return 0;
  }

  public int n() {

	 int cr;
	 int ci;
	 int zr;
	 int zi;
	 int n;
	 int x;
	 int y;
	 int t;
	 int absz;


	 y = 0;
	 while (y < 20) {
		 ci  = z0i + (y - 10) * (2*d);
		 x = 0;
		 while (x < 80) {
			cr  = z0r + (x - 40) * d;
			zr = 0;
			zi = 0;
			absz = 0;
			n = 0;
			while (absz < 4194304 && n < maxit) {
			  t = zr;
			  zr = this.shr10(zr * zr) - this.shr10(zi * zi) + cr;
			  zi = 2*this.shr10(t * zi) + ci;
			  absz = zr * zr + zi * zi;
			  n = n+1;
			}
			n = this.printdot(n);
			x = x + 1;
		 }
          System.out.print ((char)10);
		 y = y + 1;
	 }
          System.out.print ((char)10);
	 return 0;
  }

  public int shr10(int v) {
	 return v/ 1024;
  }

  public int cursorToOrigin() {
    System.out.print ((char)27);
    System.out.print ((char)91);
    System.out.print ((char)72);
    return 0;
  }

  public int printcol(int v) {
          System.out.print ((char)27);
          System.out.print ((char)91);
          System.out.print ((char)52);
          System.out.print ((char)v);
          System.out.print ((char)109);
          System.out.print((char)32);
          System.out.print ((char)27);
          System.out.print ((char)91);
          System.out.print ((char)48);
          System.out.print ((char)109);
          return 0;
  }

  public int printdot(int v) {
	 int h;
	 int p;
	 int q;
	 int t;
	 int i;
    h = 6*v*1024/maxit;
    i = 6*v/maxit;
	 t = h - i*1024;
	 q = 1024 - t;
    h = this.printcol(48 + i);
	 return v;
  }

  // don't do this at home
  public int delay() {
    int x;
    x = 0;
    while (x < 100000000) {
      x = x + 1;
    }
    return x;
  }
}
