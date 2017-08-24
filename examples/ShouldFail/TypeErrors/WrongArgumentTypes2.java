class Main {
    public static void main (String [] a) {
	System.out.println(99);
    }
}

class Foo {
    int x;
    public boolean is_foo (int n) {
	return (x<n);
    }
    public boolean all_foo (int n, int m) {
	return (this.is_foo(n) && this.is_foo(m)+1);
    }
    public int wrong (int n) {
	return this.all_foo(n,n);
    } 
}