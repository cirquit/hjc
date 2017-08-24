class Hanoi {
  public static void main(String[] args) {
    System.out.print((char)new H().init().run());
  }
}

class H {
  int n;
  int[] tower1;
  int[] tower2;
  int[] tower3;

  public H init() {
    int i;

    n = 10;
    tower1 = new int[n];
    tower2 = new int[n];
    tower3 = new int[n+1/2];

    i = 0;
    while (i < n) {
      tower1[i] = n-i;
      i = i + 1;
    }
    return this;
  }

  public int run() {
    int d;
    d = this.display();
    d = this.move(n, tower1, tower3, tower2);
    return 13;
  }

  public int move(int k, int[] from, int[] to, int[] other) {
    int i;
    int j;
    if (k < 2) {
      i = 0;
      while (i < from.length && 0 < from[i]) {
        i = i + 1;
      }
      i = i - 1;
      j = 0;
      while (0 < to[j]) {
        j = j + 1;
      }
      to[j] = from[i];
      from[i] = 0;
      i = this.display();
    } else {
      i = this.move(k-1, from, other, to);
      i = this.move(1, from, to, other);
      i = this.move(k-1, other, to, from);
    }
    return 0;
  }

  public int display() {
    int i;
    int d;
    i = 0;
    while (i < n) {
      d = this.drawbar(111, 2*tower1[n-i-1], 2*n+2);
      d = this.drawbar(111, 2*tower2[n-i-1], 2*n+2);
      d = this.drawbar(111, 2*tower3[n-i-1], 2*n+2);
      System.out.print((char)10);
      i = i + 1;
    }
    d = this.drawbar(61, 3*2*n+3*2, 3*2*n+3*2);
    System.out.print((char)10);
    System.out.print((char)10);
    System.out.print((char)10);
    return 0;
  }

  public int drawbar(int c, int w, int f) {
    int i;
    int l;
    int r;
    l = (f - w) / 2;
    r = f - w - l;
    while (0 < l) {
      System.out.print((char)32);
      l = l - 1;
    }
    while (0 < w) {
      System.out.print((char)c);
      w = w - 1;
    }
    while (0 < r) {
      System.out.print((char)32);
      r = r - 1;
    }
    return 0;
  }
}
