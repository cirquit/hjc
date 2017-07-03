// Perform Newton-Raphson iteration to compute the square-root of 
// a given number
// ---------------------------------------------------------------------------
// Expected output (with epsilon =1/100)(ie. sqrt(2) ~= 577/408)
// 2          // input
// 999        // marker
// 577        // numerator
// 408        // denominator
// 0          // result code, always 0

// The value of epsilon can be adjusted in line 214 below.
// Expected output (with epsilon =1/10000)(ie. sqrt(2) ~= 665857/470832):
// 2
// 999
// 665857
// 470832
// 0
// ---------------------------------------------------------------------------

class Newton {
    public static void main(String[] a){
	System.out.println((new NewtonClass()).test_it(2));
    }
}

class Frac {
  int num;
  int den;

  public boolean init (int x, int y) {    
    boolean ok;
    ok = true;
    num = x;
    den = y;
    return ok;
  }

  public int den () {
    return den;
  }
  public int num () {
    return num;
  }

  public int showFrac() {
    int rc;
    rc = 0;
    System.out.println(num);
    System.out.println(den);
    return rc;
  }

  //public String toString () {
  //  return ((new Integer(this.num)).toString()+"/"+(new Integer(this.den)).toString());
  //}
}

class Integral {
  public boolean eq (int x, int y) {
	return (!(x<y)) && (!(y<x));
  }

  public int div (int x, int y) {
  	int res;
	res = 0; 
  	while (!(x<y)) {
  	    res = res + 1;
	    x = x - y;
  	}
  	return res;
  }
  public int rem (int m, int n) {
    return m-n*this.div(m,n);
  }
  public int gcd_rec (int x, int y) {
	int res;
	if (x<y) {
	    res = this.gcd_rec(y,x);
	} else if (y<1) {
	    res = x;
	} else {
	    res = this.gcd_rec(y,this.rem(x,y)); 
	}
	return res;
  }
  public int gcd_iter (int x, int y) {
        int t;
	if (x<y) {
	  t = x;
	  x = y;
	  y = t;
	} else {
	}
	while (0<y) {
	    t = x;
	    x = y;
	    y = this.rem(t,y);
	}
	return x;
  }
  public int gcd (int x, int y) {
    return this.gcd_rec(x,y);
  }
  public int lcm (int x, int y) {
    int res; 
    res = y*this.div(x, this.gcd(x,y));
    return res;
  }
}

class FracOps { // extends Integral
  Integral iops;

  public int init () {
    int rc;
    iops = new Integral();
    rc = 0;
    return rc;
  }

  public Frac mkFrac(int x, int y) {
    boolean ok;
    Frac res;
    res = new Frac();
    ok = res.init(x,y);
    return res;
  }

  public Frac plus (Frac x, Frac y) {
    int lcm;
    int res_num;
    int res_den;
    Frac res;
    lcm = iops.lcm(x.den(), y.den());
    res_num = x.num()*(iops.div(lcm,x.den())) + y.num()*(iops.div(lcm,y.den()));
    res_den = lcm;
    res = this.mkFrac(res_num, res_den);
    return res;
  }

  public Frac times (Frac x, Frac y) {
    int a;
    int b;
    int res_num;
    int res_den;
    Frac res;
    a = iops.gcd(x.den(),y.num());
    b = iops.gcd(x.num(),y.den());
    res_num = iops.div(x.num(),b) * iops.div(y.num(),a);
    res_den = iops.div(x.den(),a) * iops.div(y.den(),b);   
    res = this.mkFrac(res_num, res_den);
    return res;
  }

  public Frac recip (Frac x) {
    Frac res;
    res = this.mkFrac(x.den(), x.num());
    return res;
  }

  public Frac divide (Frac x, Frac y) {
    Frac yr;
    Frac res;
    yr = this.recip(y);
    res = this.times(x,yr);
    return res;
  }

  public Frac neg (Frac x) { // assumes non-neg fraction!
    Frac res;
    res = this.mkFrac((0-1)*x.num(), x.den());
    return res;
  }

  public Frac abs_minus (Frac x, Frac y) {
    Frac res;
    if (this.lt(x,y)) {
      res = this.plus(y,this.neg(x)); // y-x;
    } else {
      res = this.plus(x,this.neg(y)); // x-y;
    }
    return res;
  }

  public boolean lt (Frac x, Frac y) {
    int lcm;
    int x0;
    int y0;
    boolean res;
    lcm = iops.lcm(x.den(), y.den());
    x0 = x.num()*(iops.div(lcm,x.den())); 
    y0 = y.num()*(iops.div(lcm,y.den()));
    res = x0<y0;
    return res;
  }
}

class NewtonClass { //  extends FracOps 
  Frac half; 
  FracOps fops;

  // compute the sqrt of an integer using Newton-Raphson iteration:
  // approximate sqrt(N) by: x(n+1) = 1/2*(x(n)+N/x(n))
  public Frac newton_step (Frac x, int n) {
    return fops.times(half,fops.plus(x,fops.divide(fops.mkFrac(n,1),x)));
  }

  public Frac newton (int n) {
    Frac x;
    Frac y;
    Frac epsilon;
    Frac delta;
    int rc;
    half = fops.mkFrac(1,2);
    epsilon = fops.mkFrac(1,100);
    x = fops.mkFrac(n,1);
    delta = x;
    while (fops.lt(epsilon, delta)) {
      y = x;
      x = this.newton_step(x,n);
      delta = fops.abs_minus(x,y);
    }
    return x;
  }

  public int test_it(int n) {
    Frac res;
    int rc;
    fops = new FracOps();
    rc = fops.init();
    res = this.newton(n);
    System.out.println(n);    // print input number: N
    System.out.println(999);  // print marker
    rc = res.showFrac();      // print fraction, approximating swrt(N)
    return rc;
  }
}

