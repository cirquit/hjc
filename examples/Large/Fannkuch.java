// Minijava version of the fannkuch benchmark from:
// http://benchmarksgame.alioth.debian.org/u64q/program.php?test=fannkuchredux&lang=java&id=1
//
// Based the above program by Jeremy Zerfas
// Based on the Ada program by Jonathan Parker and Georg Bauhaus which in turn
// was based on code by Dave Fladebo, Eckehard Berns, Heiner Marxen, Hongwei Xi,
// and The Anh Tran and also the Java program by Oleg Mazurov.
class Fannkuch {

  public static void main(String[] args) {
    System.out.print((char) (new Fannkuch1().fannkuch()));
  }
}

class Fannkuch1 {
  int NCHUNKS;
  int CHUNKSZ;
  int NTASKS;
  int n;
  int[] Fact;
  int[] maxFlips;
  int[] chkSums;

  int[] p;
  int[] pp;
  int[] count;

  public int mod(int x, int y) {
    return x - y * (x / y);
  }

  public int min(int x, int y) {
    int r;
    if (x < y) {
      r = x;
    } else {
      r = y;
    }
    return r;
  }

  public int max(int x, int y) {
    int r;
    if (x < y) {
      r = y;
    } else {
      r = x;
    }
    return r;
  }

  public int arraycopy(int[] src, int srcPos, int[] dest, int destPos, int len) {
    int i;
    i = 0;
    while (i < len) {
      dest[destPos + i] = src[srcPos + i];
      i = i + 1;
    }
    return 0;
  }

  public boolean equals(int x, int y) {
    return !(x < y) && !(y < x);
  }

  public int firstPermutation(int idx) {
    int u;
    int i;
    int j;
    int d;

    i = 0;
    while (i < p.length) {
      p[i] = i;
      i = i + 1;
    }

    i = count.length - 1;
    while (0 < i) {
      d = idx / Fact[i];
      count[i] = d;
      idx = this.mod(idx, Fact[i]);

      u = this.arraycopy(p, 0, pp, 0, i + 1);
      j = 0;
      while (j < i + 1) {
        if (j + d < i + 1) {
          p[j] = pp[j + d];
        } else {
          p[j] = pp[j + d - i - 1];
        }
        j = j + 1;
      }
      i = i - 1;
    }
    return 0;
  }

  public int incAndGet(int[] a, int i) {
    a[i] = a[i] + 1;
    return a[i];
  }

  public boolean nextPermutation() {
    int first;
    int next;
    int i;
    int j;

    first = p[1];
    p[1] = p[0];
    p[0] = first;

    i = 1;
    while (i < this.incAndGet(count, i)) {
      count[i] = 0;
      i = i + 1;
      next = p[1];
      p[0] = p[1];
      j = 1;
      while (j < i) {
        p[j] = p[j + 1];
        j = j + 1;
      }
      p[i] = first;
      first = next;
    }
    return true;
  }

  public int countFlips() {
    int u;
    int flips;
    int first;
    int lo;
    int hi;
    int t;
    boolean b;
    flips = 1;
    first = p[0];
    if (!this.equals(p[first], 0)) {
      u = this.arraycopy(p, 0, pp, 0, pp.length);
      b = true;
      while (!(!b && !!this.equals(pp[first], 0))) {
        b = false;
        flips = flips + 1;
        lo = 1;
        hi = first - 1;
        while (lo < hi) {
          t = pp[lo];
          pp[lo] = pp[hi];
          pp[hi] = t;
          lo = lo + 1;
          hi = hi - 1;
        }
        t = pp[first];
        pp[first] = first;
        first = t;
      }
    } else {
    }
    return flips;
  }

  public int runTask(int task) {
    int u;
    boolean ub;
    int idxMin;
    int idxMax;
    int i;
    boolean brk;
    int flips;
    int maxflips;
    int chksum;

    idxMin = task * CHUNKSZ;
    idxMax = this.min(Fact[n], idxMin + CHUNKSZ);

    u = this.firstPermutation(idxMin);

    maxflips = 1;
    chksum = 0;
    i = idxMin;
    brk = false;
    while (!brk) {

      if (!this.equals(p[0], 0)) {
        flips = this.countFlips();
        maxflips = this.max(maxflips, flips);
        if (this.equals(this.mod(i, 2), 0)) {
          chksum = chksum + flips;
        } else {
          chksum = chksum - flips;
        }
      } else {
      }

      i = i + 1;
      if (this.equals(i, idxMax)) {
        brk = true;
      } else {
      }

      if (!brk) {
        ub = this.nextPermutation();
      } else {
      }
    }
    maxFlips[task] = maxflips;
    chkSums[task] = chksum;
    return 0;
  }

  public int printResult(int n, int res, int chk) {
    System.out.println(chk);
    System.out.print((char) 80);
    System.out.print((char) 32);
    System.out.println(n);
    System.out.println(res);
    return 0;
  }

  public int fannkuch() {
    int u;
    int i;
    int res;
    int chk;
    int task;

    n = 11;

    Fact = new int[n + 1];
    Fact[0] = 1;
    i = 1;
    while (i < Fact.length) {
      Fact[i] = Fact[i - 1] * i;
      i = i + 1;
    }

    NCHUNKS = 150;
    CHUNKSZ = (Fact[n] + NCHUNKS - 1) / NCHUNKS;
    NTASKS = (Fact[n] + CHUNKSZ - 1) / CHUNKSZ;
    maxFlips = new int[NTASKS];
    chkSums = new int[NTASKS];

    p = new int[n];
    pp = new int[n];
    count = new int[n];

    task = 0;
    while (task < NTASKS) {
      u = this.runTask(task);
      task = task + 1;
    }

    res = 0;
    i = 0;
    while (i < maxFlips.length) {
      res = this.max(res, maxFlips[i]);
      i = i + 1;
    }
    chk = 0;
    i = 0;
    while (i < chkSums.length) {
      chk = chk + chkSums[i];
      i = i + 1;
    }

    u = this.printResult(n, res, chk);
    return 13;
  }
}
