// test selected numbers whether they are prime
// variant using long (>4) arg list
// prints sequence of test value and int-encoded isprime?
// expected output:
// 1
// 0
// 2
// 1
// 3
// 1
// 4
// 0
// 5
// 1
// 10
// 0
// 89
// 1
// 999

class ManyArgs {
    public static void main(String[] a){
      System.out.println((new Prime()).test_it(1,2,3,4,5,10,89));
    }
}

class Prime {
    public boolean or (boolean a, boolean b) {
      return !(!a && !b);
    }
    public boolean divides (int m, int n) {
      int i;
      boolean found;
      i=1;
      found=false;
      while (!found && i<n) {
	if ((i*m)<n) {
	  i=i+1;
	} else if (n<(i*m)) {
	  i=n;
	} else {
	  found=true;
	}
      }
      return found;
    }

    // naive prime-test, without array of primes or sqrt-bound
    public boolean isprime (int n) {
      int i;
      boolean ok;
      ok = this.or((1<n) && (n<3), ((2<n) && !this.divides(2,n)));
      i=3;
      while (ok && (i<n)) {
	ok = ok && !(this.divides(i,n));
	i=i+2;
      }
      return ok;
    }

  // boolean to int conversion
  public int b2i (boolean b) {
    int res;
    if (b) {
      res = 1;
    } else {
      res = 0;
    }
    return res;
  }

  public int test_it (int x1, int x2, int x3, int x4, int x5, int x6, int x7) {
      int n;
      boolean b;
      n = x1;
      System.out.println(n);
      b = this.isprime(n);
      System.out.println(this.b2i(b));
      // ---
      n = x2;
      System.out.println(n);
      b = this.isprime(n);
      System.out.println(this.b2i(b));
      // ---
      n = x3;
      System.out.println(n);
      b = this.isprime(n);
      System.out.println(this.b2i(b));
      // ---
      n = x4;
      System.out.println(n);
      b = this.isprime(n);
      System.out.println(this.b2i(b));
      // ---
      n = x5;
      System.out.println(n);
      b = this.isprime(n);
      System.out.println(this.b2i(b));
      // ---
      n = x6;
      System.out.println(n);
      b = this.isprime(n);
      System.out.println(this.b2i(b));
      // ---
      n = x7;
      System.out.println(n);
      b = this.isprime(n);
      System.out.println(this.b2i(b));
      // ---
      // System.out.println(999);
      return 999;
  }
}
