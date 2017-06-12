// type error in function invocation

class WrongArgumentTypes {
  public static void main (String[] a) {
    System.out.println((new A()).do_it(1,true));
                                   // ^^^^^^^^ type mis-match in argument list
  }
}

class A {
  public int do_it (int x, int y) {
    int res;
    res = x+y;
    return res;
  }
}
