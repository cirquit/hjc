// Prints "0".  And nothing else!!!

class ShortCutAnd {
    public static void main (String[] argv) {
	System.out.println(new TestAnd().run(false));
    }
}

class TestAnd {

    public int run(boolean b) {
	int result;
        if (b && this.sideEffect()) result = 1;
        else result = 0;
	return result;
    }

    public boolean sideEffect() {
	System.out.println(0-9999);
	System.out.println(0-9999);
	System.out.println(0-9999);
	System.out.println(0-9999);
	System.out.println(0-9999);
        return true;         
    }

}