// Berechung einiger Stellen von Pi.
// Geschrieben von Stephan Barth.

class PiSin {
  public static void main(String[] args) {
    System.out.print((char)(new PiDigits().pi()));
  }
}

class PiDigits {

  // Welchen Wert pro Stelle?
  public int base() {
    return 1000;
  }

  // Nachkommastellen
  public int places() {
    return 40;
  }

  // Hinten Reserve zum Runden
  public int reserve() {
    return 5;
  }

  public boolean equals(int x, int y) {
    return !(x < y) && !(y < x);
  }

  public int mod(int x, int y) {
    return x - y * (x / y);
  }

  public int div(int i,int j) {
    int ret;
    if(this.equals(j, 0)){ // Geteilt durch 0 gibt bei uns 0
      ret = 0;
    } else {
      ret = 0;
      while(0 < i - j + 1){
        ret = ret + 1;
        i = i - j;
      }
    }
    return ret;
  }

  public int shownum(int[] num) {
    int i;
    i=0;
    while(i<this.places() - this.reserve()){
      System.out.print((char)(48 + num[i]/100));
      System.out.print((char)(48 + this.mod(num[i]/10, 10)));
      System.out.print((char)(48 + this.mod(num[i], 10)));
      if(this.equals(0, i)){
        System.out.print((char)46);
      }else{
        System.out.print((char)32);
      }
      i = i + 1;
    }
    System.out.print((char)10);
    System.out.print((char)13);
    return 0;
  }

  // Alle Stellen auf 0..base-1 bringen
  public int overflow(int[] n) {
    int i;
    i = this.places()-1;
    while(0 < i){
      while(this.base() < n[i] + 1){
        n[i]=n[i]-this.base();
        n[i-1] = n[i-1] + 1;
      }
      while(n[i]<0){
        n[i]=n[i]+this.base();
        n[i-1] = n[i-1] - 1;
      }
      i = i - 1;
    }
    return 0;
  }

  // i <- j + k
  public int add(int[] i, int[] j, int[] k) {
    int n;
    n =0;
    while(n<this.places()){
      i[n] = j[n]+k[n];
      n = n + 1;
    }
    return 0;
  }

  // i <- j - k
  public int sub(int[] i, int[] j, int[] k) {
    int n;
    n=0;
    while(n<this.places()){
      i[n] = j[n]-k[n];
      n = n + 1;
    }
    return 0;
  }

  // i <- -j
  public int neg(int[] i, int[] j) {
    int n;
    n=0;
    while(n<this.places()){
      i[n] = 0-j[n];
      n = n + 1;
    }
    return 0;
  }

  // i <- j/k
  public int longdiv(int[] i, int[] j, int k) {
    int u;
    int n;
    int d;
    int rem;
    u = this.overflow(j);
    n=0;
    i[0]=0;
    while(n<this.places()-1){
      i[n]=i[n]+j[n];
      d = this.div(i[n], k);
      rem = i[n] - d*k;
      i[n]=d;
      i[n+1]=rem*this.base();
      n = n + 1;
    }
    u = this.overflow(i);
    return 0;
  }

  // i <- j * k; i darf weder j noch k sein
  public int mult(int[] i, int[] j, int[] k) {
    int u;
    int n;
    int s;
    int t;
    u = this.overflow(j);
    u = this.overflow(k);
    n=0;
    while(n<this.places()){
      i[n]=0;
      n = n + 1;
    }
    s=0;
    while(s<this.places()){
      t=0;
      while(s+t<this.places()){
        i[s+t] = i[s+t] + j[s]*k[t];
        t = t + 1;
      }
      s = s + 1;
    }
    u = this.overflow(i);
    return 0;
  }

  // i <- j
  public int mv(int[] i, int[] j) {
    int n;
    n = 0;
    while(n<this.places()){
      i[n]=j[n];
      n = n + 1;
    }
    return 0;
  }

  public int nullify(int[] n) {
    int i;
    i = 0;
    while(i<this.places()){
      n[i]=0;
      i = i + 1;
    }
    return 0;
  }

  public boolean iszero(int[] n){
    int i;
    boolean r;
    i = 0;
    r = true;
    while(r && i<this.places()-this.reserve()){
      if(!this.equals(n[i],0)) {
        r = false;
      } else {}
      i = i + 1;
    }
    return r;
  }

  // i <- sin j; temoraere Werte werden in fac und tmp gespeichert
  public int sin(int[] i, int[] j, int[] fac, int[] tmp) {
    int u;
    int k;

    u = this.overflow(j);
    u = this.mv(fac, j);
    u = this.nullify(i);
    k = 1;
    while(!this.iszero(fac)){
      //
      u = this.add(i,i,fac);
      u = this.mult(tmp, fac, j);
      u = this.longdiv(fac, tmp, k+1);
      //
      u = this.mult(tmp, fac, j);
      u = this.longdiv(fac, tmp, k+2);
      //
      u = this.sub(i,i,fac);
      u = this.mult(tmp, fac, j);
      u = this.longdiv(fac, tmp, k+3);
      //
      u = this.mult(tmp, fac, j);
      u = this.longdiv(fac, tmp, k+4);
      k=k+4;
    }
    u = this.overflow(i);
    return 0;
  }

  public int pi() {
    int u;
    int[] a;
    int[] b;
    int[] c;
    int[] pi;
    int k;
    a = new int[this.places()];
    b = new int[this.places()];
    c = new int[this.places()];
    pi = new int[this.places()];
    u = this.nullify(a);
    u = this.nullify(b);
    u = this.nullify(c);
    u = this.nullify(pi);
    pi[0]=1;
    k = 0;
    while(k<10){
      u = this.overflow(pi);
      u = this.shownum(pi);
      u = this.sin(a,pi,b,c);
      u = this.add(pi,pi,a);
      k = k + 1;
    }
    return 13;
  }
}
