class VVST {
    public static void main(String[] a){
        System.out.println(new Test().run());
    }
}

class Test {

    int x;
    int y;
    int z;

    public void setup(){
        x = 20;
    }

    public int run(){
        // int a = 10;
        y = 10;
        this.setup();
        return x + y / 5 + z;
    } 
}
