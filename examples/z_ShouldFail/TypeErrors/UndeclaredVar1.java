// non-existing: class var

class UndeclaredVar1 {
  public static void main (String[] a) {
    System.out.println((new A()).getVar());
  }
}

class A {
  int a;
  public int getVar() {
    int res;
    res = x;
    //    ^^var does not exist
    return res;
  }
}
