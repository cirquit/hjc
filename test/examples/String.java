class Add {
    public static void main(String[] a) {
            System.out.println("new Test().add()");
    }
}

class Test{
    String[] str;
    public int add()
    {
     str = new String[1];
     str[0] = "Blub!";
     return 1;
    }

    public void test() {}
}
