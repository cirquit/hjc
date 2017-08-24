// non-existing: class var

class UndeclaredType {
  public static void main (String[] a) {
    System.out.println((new A()).do_it(1));
  }
}

class A {
  B b;
  // ^^ type does not exist
  public int do_it (int x) {
    int res;
    res = x;
    return res;
  }
}
