class Main {
    public static void main (String [] a) {
	System.out.println(99);
    }
}

class Foo {
    int x;
    public boolean is_foo (int n,) {
//                          ^^^^^^  ParamList shouldn't end with ,
	return (x<n);
    }
}