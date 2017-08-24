// Variante von pidigits.c aus dem "Computer Language Benchmarks Game" mit
// naiver BigInt-Klasse.
//
// Klasse PiDigits basiert auf:
// https://benchmarksgame.alioth.debian.org/u64q/program.php?test=pidigits&lang=gcc&id=1
// (Contributed by Mr Ledrug)

class Pi {
  public static void main(String[] args) {
    System.out.print((char) (new PiDigits().init().pi()));
  }
}

class Stream {

  // invariants:
  // - size is the smallest number, so that all following numbers are zero
  // - elements.length > 0
  int size;
  int[] elements;

  public Stream init(int capacity) {
    size = 0;
    elements = new int[capacity + 1];
    return this;
  }

  public int size() {
    return size;
  }

  public Stream copy(Stream x) {
    int u;
    int i;
    size = x.size();
    u = this.resizeArray(x.size() - 1);

    i = 0;
    while (i < elements.length) {
      elements[i] = x.get(i);
      i = i + 1;
    }
    return this;
  }

  public int clear() {
    int i;
    i = 0;
    while (i < elements.length) {
      elements[i] = 0;
      i = i + 1;
    }
    size = 0;
    return 0;
  }

  public int get(int i) {
    int v;
    if (i < elements.length) {
      v = elements[i];
    } else {
      v = 0;
    }
    return v;
  }

  public int set(int i, int x) {
    int u;
    int y;
    if (!(!(0 < x) && !(i < elements.length))) {
      u = this.resizeArray(i);
      y = elements[i];
      elements[i] = x;
      if (size < i + 1) {
        size = i + 1;
      } else {
      }
      u = this.shrink();
    } else {
      y = 0;
    }
    return y;
  }

  public int shrink() {
    while (0 < size && elements[size - 1] < 1) {
      size = size - 1;
    }
    return 0;
  }

  public int resizeArray(int idx) {
    int i;
    int l;
    int[] a;

    l = elements.length;
    while (!(idx < l)) {
      l = l * 2;
    }
    if (elements.length < l) {
      a = new int[l];
      i = 0;
      while (i < elements.length) {
        a[i] = elements[i];
        i = i + 1;
      }
      elements = a;
    } else {
    }
    return 0;
  }
}

class BigInt {

  int sign; // 1 oder -1
  Stream digits;

  public int base() {
    return 65536;
  }

  public BigInt initZero(int capacity) {
    sign = 1;
    digits = new Stream().init(capacity);
    return this;
  }

  public BigInt initCopy(BigInt x) {
    BigInt u;
    sign = 1;
    digits = new Stream().init(x.size());
    u = this.copy(x);
    return this;
  }

  public BigInt initValue(int x) {
    int u;
    u = this.initZero(10).setValue(x);
    return this;
  }

  public int setValue(int x) {
    int u;
    int i;
    int absx;
    if (x < 0) {
      sign = 0 - 1;
    } else {
      sign = 1;
    }
    absx = sign * x;
    u = digits.clear();
    i = 0;
    while (0 < absx) {
      u = digits.set(i, this.mod(absx, this.base()));
      absx = absx / this.base();
      i = i + 1;
    }
    return 0;
  }

  public BigInt copy(BigInt x) {
    Stream u;
    sign = x.getSign();
    u = digits.copy(x.getDigits());
    return this;
  }

  public BigInt share(BigInt x) {
    sign = x.getSign();
    digits = x.getDigits();
    return this;
  }

  public BigInt shareNegate() {
    int u;
    BigInt r;
    r = new BigInt().share(this);
    u = r.setSign(0 - sign);
    return r;
  }

  public int getSign() {
    return sign;
  }

  public int setSign(int s) {
    sign = s;
    return 0;
  }

  public Stream getDigits() {
    return digits;
  }

  public int getLastDigit() {
    return digits.get(0);
  }

  public int size() {
    return digits.size();
  }

  public boolean lt(BigInt x) {
    boolean lt;
    if (sign < x.getSign()) {
      lt = !(this.isZero() && x.isZero());
    } else if (x.getSign() < sign) {
      lt = !(this.isZero() && x.isZero());
    } else if (0 < sign) {
      lt = this.compareAbs(x) < 0;
    } else {
      lt = x.compareAbs(this) < 0;
    }
    return lt;
  }

  public int compareAbs(BigInt x) {
    int i;
    int r;
    i = this.max(digits.size(), x.size()) - 1;
    while (0 - 1 < i && this.equals(digits.get(i), x.getDigits().get(i))) {
      i = i - 1;
    }
    if (0 - 1 < i) {
      if (digits.get(i) < x.getDigits().get(i)) {
        r = 0 - 1;
      } else {
        r = 1;
      }
    } else {
      r = 0;
    }
    return r;
  }

  public boolean isZero() {
    int i;
    boolean z;
    i = digits.size() - 1;
    while (0 - 1 < i && this.equals(digits.get(i), 0)) {
      i = i - 1;
    }
    return i < 0;
  }

  public int shr(int k) {
    int u;
    int i;
    int s;
    s = this.size();
    i = 0;
    while (i < s - k) {
      u = digits.set(i, digits.get(i + k));
      i = i + 1;
    }
    while (i < s) {
      u = digits.set(i, 0);
      i = i + 1;
    }
    return 0;
  }

  public int shl(int k) {
    int u;
    int i;
    int s;
    s = this.size();
    i = s - 1;
    while (0 - 1 < i) {
      u = digits.set(i + k, digits.get(i));
      i = i - 1;
    }
    i = k - 1;
    while (0 - 1 < i) {
      u = digits.set(i, 0);
      i = i - 1;
    }
    return 0;
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

  public int mod(int x, int y) {
    return x - y * (x / y);
  }

  public boolean equals(int x, int y) {
    return !(x < y) && !(y < x);
  }

}

class Ops {

  public int add(BigInt r, BigInt y, BigInt x) {
    int u;
    Stream rd;
    int l;
    int i;
    int carry;
    int sum;
    int s;

    u = r.setValue(0);
    if (y.getSign() < x.getSign()) {
      u = this.sub(r, y.shareNegate(), x);
      u = r.setSign(0 - r.getSign());
    } else if (x.getSign() < y.getSign()) {
      u = this.sub(r, y, x.shareNegate());
    } else {
      u = r.setValue(0);
      u = r.setSign(y.getSign());

      l = this.max(y.size(), x.size());
      i = 0;
      carry = 0;
      while (i < l) {
        s = y.getDigits().get(i) + x.getDigits().get(i) + carry;
        sum = this.mod(s, x.base());
        carry = (s - sum) / x.base();
        u = r.getDigits().set(i, sum);
        i = i + 1;
      }
      u = r.getDigits().set(i, carry);
    }
    return 0;
  }

  public int sub(BigInt r, BigInt y, BigInt x) {
    int u;

    u = r.setValue(0);
    if (y.getSign() < x.getSign()) {
      u = this.add(r, y.shareNegate(), x);
      u = r.setSign(0 - r.getSign());
    } else if (x.getSign() < y.getSign()) {
      u = this.add(r, y, x.shareNegate());
    } else if (y.compareAbs(x) < 0) {
      u = this.subAbsSmaller(r, x, y);
      u = r.setSign(0 - y.getSign());
    } else {
      u = this.subAbsSmaller(r, y, x);
      u = r.setSign(y.getSign());
    }
    return 0;
  }


  public int subAbsSmaller(BigInt r, BigInt y, BigInt x) {
    int u;
    int l;
    int i;
    int sum;
    int carry;
    BigInt ubi;
    BigInt tmp;
    int s;

    i = 0;
    carry = 0;
    while (i < y.size()) {
      s = (x.base() - 1 - y.getDigits().get(i)) + x.getDigits().get(i) + carry;
      sum = this.mod(s, x.base());
      carry = (s - sum) / x.base();
      u = r.getDigits().set(i, x.base() - 1 - sum);
      i = i + 1;
    }
    return 0;
  }

  public int div(BigInt r, BigInt y, BigInt x) {
    int u;
    BigInt ubi;
    BigInt dividend;
    BigInt divisor;
    BigInt tmp;
    int d;
    int i;

    u = r.setValue(0);
    tmp = new BigInt().initZero(y.size());
    dividend = new BigInt().initCopy(y);
    u = dividend.setSign(1);
    divisor = new BigInt().initCopy(x);
    u = divisor.setSign(1);

    i = y.size() - x.size();
    if (0 - 1 < i) {
      u = divisor.shl(i);
      while (0 - 1 < i) {
        d = 0;
        while (divisor.compareAbs(dividend) < 1) {
          u = this.sub(tmp, dividend, divisor);
          ubi = dividend.copy(tmp);
          d = d + 1;
        }
        u = r.getDigits().set(i, d);
        u = divisor.shr(1);
        i = i - 1;
      }
      u = r.setSign(y.getSign() / x.getSign());
    } else {
    }
    return 0;
  }

  public int mul(BigInt r, BigInt x, BigInt y) {
    int u;
    Stream a;
    Stream b;
    Stream rd;
    int i;
    int ai;
    int bi;
    int d;
    int carry;
    int sum;
    int s;

    a = x.getDigits();
    b = y.getDigits();
    u = r.setValue(0);
    u = r.setSign(x.getSign() * y.getSign());
    rd = r.getDigits();

    bi = 0;
    while (bi < b.size()) {
      carry = 0;
      ai = 0;
      while (ai < a.size()) {
        d = rd.get(ai + bi) + carry + a.get(ai) * b.get(bi);
        carry = d / x.base();
        u = rd.set(ai + bi, this.mod(d, x.base()));

        ai = ai + 1;
      }
      d = rd.get(bi + a.size()) + carry;
      u = rd.set(bi + a.size(), d);

      bi = bi + 1;
    }
    return 0;
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

  public int mod(int x, int y) {
    return x - y * (x / y);
  }
}

class PiDigits {
  Ops ops;
  BigInt tmp1;
  BigInt tmp2;
  BigInt acc;
  BigInt den;
  BigInt num;

  public PiDigits init() {
    ops = new Ops();
    tmp1 = new BigInt().initZero(10);
    tmp2 = new BigInt().initZero(10);
    acc = new BigInt().initZero(10);
    den = new BigInt().initZero(10);
    num = new BigInt().initZero(10);
    return this;
  }

  public int extract_digit(int nth) {
    int u;
    u = tmp2.setValue(nth);
    u = ops.mul(tmp1, num, tmp2);
    u = ops.add(tmp2, tmp1, acc);
    u = ops.div(tmp1, tmp2, den);

    return tmp1.getLastDigit();
  }

  public int eliminate_digit(int d) {
    int u;
    BigInt ubi;

    u = tmp2.setValue(d);
    u = ops.mul(tmp1, den, tmp2);
    u = ops.sub(tmp2, acc, tmp1);
    ubi = acc.copy(tmp2);

    u = tmp2.setValue(10);
    u = ops.mul(tmp1, acc, tmp2);
    ubi = acc.copy(tmp1);
    u = ops.mul(tmp1, num, tmp2);
    ubi = num.copy(tmp1);
    return 0;
  }

  public int next_term(int k) {
    int u;
    BigInt ubi;
    int k2;

    k2 = k * 2 + 1;

    u = tmp2.setValue(2);
    u = ops.mul(tmp1, num, tmp2);
    u = ops.add(tmp2, acc, tmp1);
    ubi = acc.copy(tmp2);

    u = tmp2.setValue(k2);
    u = ops.mul(tmp1, acc, tmp2);
    ubi = acc.copy(tmp1);
    u = ops.mul(tmp1, den, tmp2);
    ubi = den.copy(tmp1);
    u = tmp2.setValue(k);
    u = ops.mul(tmp1, num, tmp2);
    ubi = num.copy(tmp1);
    return 0;
  }

  public int pi() {
    int u;
    int d;
    int d1;
    int k;
    int i;
    int n;
    n = 1000;

    u = acc.setValue(0);
    u = den.setValue(1);
    u = num.setValue(1);

    i = 0;
    k = 0;
    while (i < n) {
      k = k + 1;
      u = this.next_term(k);
      if (!acc.lt(num)) {

        d = this.extract_digit(3);
        d1 = this.extract_digit(4);
        if (!(d < d1) && !(d1 < d)) {
          System.out.print((char) (48 + d));
          i = i + 1;
          if (i < (i / 80) * 80 + 1) {
            System.out.print((char) 10);
            System.out.print((char) 13);
          } else {
          }
          u = this.eliminate_digit(d);
        } else {
        }
      } else {
      }
    }
    return 13;
  }
}
