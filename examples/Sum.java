// expected output: 15

class Sum {
    public static void main(String[] a){
      System.out.println((new SumClass()).f(0,5)); // f(x,y) = x + sum{i=1..y) i
    }
}

class SumClass {
 int a; // remember last sum encountered in f
 public int f (int x, int y) {
   int z;
   int res;
   z=x+y;
   a=this.g(z);
   if (y<1) {
     res = z;
   } else {
     res = this.f(z,y-1);
   }
   return res;
 }
 public int g (int x) {
   return x;
 }
}
