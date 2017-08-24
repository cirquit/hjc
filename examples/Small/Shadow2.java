class Shadow1 {
    public static void main(String[] a) {
        System.out.println(new Test().blub());
    }
}

class Test {
    // MiniJava Extention: we initialize variable to 0 by default
    int i; 

    public int blub(){
        int i = 10;
        return this.i;
    }
}

class Test {
    int i;

    public int blub(int i){
        int i = 10;
        return i;
    }
}
