class ShortCutReturn {
    public static void main(String[] a) {
        System.out.println(new Test().blub());
    }
}

class Test {
    public int blub() {
        if (1 == 1) {
            return 1;
        }

        return 2;
    }
}
