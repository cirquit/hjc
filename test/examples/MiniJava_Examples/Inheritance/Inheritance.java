
// Program should return "1" and "10":
// - A.init sets A.f1 to 10
// - A.init calls the instance B.m, which writes 1,
//   because "this" is of dynamic class B
// - f1 assigned in B.m is shadowing f1 of class A, 
//   so A.init returns the unchanged A.f1


class Inheritance {
    public static void main(String[] a){
	System.out.println(new B().init());
    }
}

class A {
    int f1;

    public int init() {
        A a;
	int tmp;

	f1 = 10;
        a = this.m();
	return f1;
    }

    public A m() {
	f1 = 20;
	System.out.println(0);
	return this;
    }
}


class B extends A {

    int f1;

    public B m() {
	f1 = 30;
	System.out.println(1);
        return this;
    }
}
