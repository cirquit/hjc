// compute a Fibonacci number
// More efficient version using an array as lazy memo table.
// expected output:
// 1973
// -----------------------------------------------------------------------------

class FibL {
    public static void main(String[] a){
	System.out.println(new FibAux().nfib_lazy(15));
    }
}

class FibAux {
    public int nfib (int num){
	int res ;
	if (num < 2)
	    res = 1 ;
	else
	    res = (this.nfib(num-1))+(this.nfib(num-2))+1 ;
	return res ;
    }
    public int nfib_lazy (int n){
      int res;
      boolean q;
      LazyArray l;
      if (n<1) {
	  res = 0;
      } else {
	  res = 0;
      }
      l = new LazyArray();
      q = l.init(n);
      res = l.nfib(n);
      return res;
    }
}

class LazyArray {
  int [] table;
  public boolean init (int n) {
      int i;
      table = new int[n];
      i=0;
      while (i<n) {
	  table[i] = 0;
          i=i+1;
      }
      return true;
  }
  public boolean defined (int x) { // undefined iff /=0
      boolean res;
      if (x<0) {
	  res = true;
      } else if (0<x) {
	  res = true;
      } else {
	  res = false;
      }
      return res;
  }

  public int get (int n) {
      int x;
      int y;
      int res;
      if (n<1) {
	  res = 0;
      } else {
	  res = 0;
      }
      x = table[n];
      if (this.defined(x)) {
	  res = x;
      } else {
	  y = this.f(n);
	  table[n] = y;
	  res = y;
      }
      return res;
  }
  public int f (int n) {
      return this.nfib(n);
  }
  public /* actually, private */ int nfib (int n){
      int res;
      if (n<2) {
	  res = 1;
      } else {
	  res = this.get(n-1)+this.get(n-2)+1;
      }
      return res;
  }
}

