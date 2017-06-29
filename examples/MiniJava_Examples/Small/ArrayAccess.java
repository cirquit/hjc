
class ArrayAccess {
    public static void main (String[] argv) {
	System.out.println (new AA().run());
    }
}

class AA {

    public int run () {

        int[] arr;

        arr  = new int[2];

        arr[0] = 5;
        arr[1] = 10;
        return arr[0];
 
    }

}