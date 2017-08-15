class AnimalInheritance {
    public static void main(String[] a) {
            System.out.println(new Dog().doubleMeter);
    }
}

class Animal {

    int simpleMeter;

    public void go(int meter) {
        simpleMeter = simpleMeter + meter;
    }
}

class Dog extends Animal {

    int doubleMeter;

    public void run(int meter) {
        this.go(meter * 2);
        doubleMeter = this.simpleMeter;
    }
}

class Puppy extends Dog {

    int centimeter;

    public int crawl(int centimeter) {
        this.go(centimeter / 100);

        centimeter = this.simpleMeter / 100;
    }
}


