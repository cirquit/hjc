// sum over an array
// expected output for sum 1+...+10:
// 55
// if array bounds checking is implemented, this program should abort
// ---------------------------------------------------------------------------

class ArrSumBUG {
    public static void main(String[] a){
	System.out.println((new Arr()).do_it(10));
    }
}

class Arr {
    int[] theArr;
    int mx;

    // theArr[i-1]=i, for all 1<=i<m
    public int init(int m) {
	int i;
	mx = m;
	i = 1;
	while (i<mx+1) {
	    theArr[i-1] = i;
	    i = i + 1;
	}
	return 0;
    }
    // Sum_{0<=i<m} i
    public int sum() {
	int i;
	int s;
	i = 1;
	s = 0;
	while (i<mx+1) { // for i=1..mx
	  s = s + theArr[i]; // BUG! should be: i-1
	  i = i + 1;
	}
	return s;
    }
    public int do_it(int m) {
	int q;
	theArr = new int[m];
	q = this.init(m);
	return this.sum();
    }
}
