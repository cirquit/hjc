// Test correct translation of side effects

class Effects {
    public static void main (String[] argv) {
        System.out.println(new EffectsClass().run());
    }
}

class EffectsClass {

    public int run () {
	Bit b;
        b = new Bit();
        return ((b.set(1) - 1) - (b.set(0) - (0 - b.value())));
        // returns 0
    }
}

class Bit {

    int bit;

    public int set (int b) {
	bit = b;
	return b;
    }

    public int value () {
	return bit;
    }

}