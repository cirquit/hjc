class While {
    public static void main (String[] argv) {
	System.out.println(new WhileClass().run());
    }
}

class WhileClass {

    int sum;
    int i;
    public int run () {

        sum = 0;
        i = 1;
	while (i < 11) {
	    sum = sum + i;
	    System.out.println (sum);
            i = i + 1;
        }
	return 0;
    }
}
