class Add {
    public static void main(String[] a) {
        AddM obj = new AddM();
        obj.y = 2;
        System.out.println(obj.add(4));
    }
}

class AddM {
// 
    int y;

    public int add(int i){
        int x = 10;
        x = x + i;
        y = 21;
        return x - y;
    }

}
