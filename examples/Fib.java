// compute a Fibonacci number
// expected output for n=15:
// 1973
// -----------------------------------------------------------------------------

class Fib {
    public static void main(String[] a){
	System.out.println(new FibAux().nfib(15));
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
}
