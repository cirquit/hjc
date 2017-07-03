// non-existing: method var

class UndeclaredVar2 {
  public static void main (String[] a) {
    System.out.println((new A()).do_it(1));
  }
}

class A {
  public int do_it (int x) {
    int res;
    res = x+y;
    //     ^^ var does not exist
    return res;
  }
}
