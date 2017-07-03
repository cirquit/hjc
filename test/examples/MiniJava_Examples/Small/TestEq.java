class TestEq {
    public static void main (String[] argv) {
        System.out.println(new TestEqMain().run());
    }
}

class TestEqMain {
    public int run() {
      boolean b;
      b = 4 < 4 + 1 && 4 < 4 + 1;
      if (b) {
	  System.out.println(1);
      } else {
	  System.out.println(0);
      }
      return 0;
    }
}
