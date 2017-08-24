class MethodCallWrong {
    public static void main (String[] argv) {
        System.out.println(new BLA().blaa());
    }
}

class BLA {

    public int blaa () {
        return this.bla (5, new BLA());
    }

    public int bla (BLA b, int k) {
        return k;
    }
}