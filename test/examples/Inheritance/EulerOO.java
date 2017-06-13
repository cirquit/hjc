/* Expect:
4
12
92
0
 */

// uses subclasses with static methods

class EulerOO {
    public static void main(String[] a){
	System.out.println((new Euler()).test_it());
    }
}

// euler n = length (filter (relprime n) [1..(n-1)])
class Integral {
    public boolean eq (int x, int y) {
	return (!(x<y)) && (!(y<x));
    }
    public int div (int x, int y) {
	int res;
	if (x<y) {
	    res = 0;
	} else if (this.eq(y,1)) {
	    res = x;
	} else {
	    res = 1 + this.div((x-y),y);
	}
	return res;
    }
    public int rem (int m, int n) {
	return (n-m*this.div(n,m));
    }
    public boolean divides (int m, int n) {
	return this.eq(n,m*this.div(n,m));
    }
    public int hcf (int x, int y) {
	int res;
	if (x<y) {
	    res = this.hcf(y,x);
	} else if (y<1) {
	    res = x;
	} else {
	    res = this.hcf(y,x-y);
	}
	return res;
    }
}

class Euler extends Integral {
    public boolean relprime (int m, int n) {
      return (this.hcf(m,n)<2);
    }
    public int euler (int n) {
	int s;
	int i;
	s = 0;
	i = 1;
	while (i<n) {
	    if (this.relprime(n,i)) {
	      s = s+1;
	    } else {
		// nothing
	    }
	    i = i+1;
	}
	return s;
    }
    public int test_it () {
	int y;
	boolean ok;
	int rc;
	ok = true;
	y = this.euler(5);
	System.out.println(y);
	ok = ok && this.eq(y,4);
	y = this.euler(21);
	System.out.println(y);
	ok = ok && this.eq(y,12);
	y = this.euler(188);
	System.out.println(y);
	ok = ok && this.eq(y,92);
	if (ok) {
	  rc = 0;
	} else {
	  rc = 1;
	}
	return rc;
    }
}

