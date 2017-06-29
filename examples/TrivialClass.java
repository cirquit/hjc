class TrivialClass {
    public static void main(String[] a){
	System.out.println(new C().value());
    }
}

class C {
  
  int v;

  public int value() {
    v = 555;
    return v;
  }

}