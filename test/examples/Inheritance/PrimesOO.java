// print a list of prime numbers; uses an array and subclasses

class PrimesOO {
    public static void main(String[] a){
	System.out.println((new Primes()).test_it(22));
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
    public boolean dividess (int m, int n) {
	return this.eq(n,m*this.div(n,m));
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

class Primes extends Integral {
    int [] primes;
    int len;

    public boolean isprime (int n) {
      int i;
      boolean ok;
      ok = true;
      i=0;
      while (ok && (i<len) && (i<n)) {
	ok = ok && !(this.divides(primes[i],n));
	i=i+1;
      }
      return ok;
    }
	
    public int primes (int n) {
	int s;
	int i;
	s = 0;
	i = 2;
	while (i<n) {
	    if (this.isprime(i)) {
	      primes[len]=i;
	      len=len+1;
	      System.out.println(i);
	      i=i+1;
	      s=s+1;
	    } else {
	      i=i+1;
	    }
	}
	return s;
    }

    public int test_it (int n) {
      int s;
      primes = new int [n];
      len = 0;
      s = this.primes(n);
      System.out.println(999);
      return s;
    }
}
