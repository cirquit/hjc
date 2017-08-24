class Stck {
    public static void main(String[] a){
	System.out.println((new StckClass()).stckme(55));
    }
}

class StckClass {
 public int stckme (int x) {
   int loc;
   int dummy;
   loc = x;
   if (x<1) {
     // nothing
   } else {
      dummy = this.stckme(x-1);
   }
   return loc;
 }
}
