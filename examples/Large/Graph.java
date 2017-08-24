// Mini-Java program "Graph"
// Andreas Abel, 2006-01-16
// Version with single-line comments only
// 
// 
// 
// Directed Graphs with bounded degree (here, two).
// 
// For unbounded degree, one could insert fake nodes which only serve to
// increase the outdegree.



class Graph {
    public static void main (String[] argv) {
        System.out.println(new Test().test());
    }
}


class Label {
    
    int value;
    
    public int init(int v) {
        value = v;
        return 0;
    }
    public int label () {
        return value;
    }

    public boolean eq(Label l) {
        int v2;
        v2 = l.label();
        return (value < v2 + 1 && v2 < value + 1);
    }

    public int print () {
        System.out.println(value);
        return 0;
    }
}


class Node {

    Label a;
    boolean visited;
    Node left;
    Node right;

    // init node 
    public int init(Label b) {
        visited = false;
        a = b;
        left = this;
        right = this;
        return 0;
    }

    public Label label() {
        return a;
    }

    public int setLeft(Node n) {
        left = n;
        return 0;
    }

    public int setRight(Node n) {
        right = n;
        return 0;
    }

    public int print() {
        int bla;
        bla = a.print();
        return 0;
    }

    public boolean beq(boolean a, boolean b) { // shadowing a
        return (!(!(a && b) && !(!a && !b)));
    }

    public boolean dfs (boolean vis, Label b) {
        boolean result;
        if (this.beq(visited,vis)) { result = false; }
        else {
            if (a.eq(b)) { result = true; }
            else {
                visited = vis;
                if (left.dfs(vis,b)) result = true;
                else if (right.dfs(vis,b)) result = true;
                else result = false;
            }
        }
        return result;
    }

    
}

class Test {

    public Label newLabel (int v) {

        int bla; // dummy var for turning expression into statement

        Label l;
        l = new Label();
        bla = l.init(v);
        return l;
    }

    public Node newNode (Label l) {

        int bla; // dummy var for turning expression into statement

        Node n;
        n = new Node();
        bla = n.init(l);
        return n;
    }

    public int findPath (Node s, Node t) {

        int bla; // dummy var for turning expression into statement

        boolean res; boolean tmp;

        bla = s.print();
        bla = t.print();
        res = s.dfs(true,t.label());       // while searching set visited
        tmp = s.dfs(false,t.label());      // reset visited by going same way
        if (res) System.out.println (999); // YES
        else System.out.println (0 - 999); // NO

        return 0;
    }

    public int test () {

        int bla; // dummy var for turning expression into statement

        boolean res1;
        boolean res2;


        // create the following Graph
        //
        // 1 --> 2 --> 3 --> 1 --> 4    5
        //             3 --> 4
        

        Label l1; Node n1;
        Label l2; Node n2;
        Label l3; Node n3;
        Label l4; Node n4;
        Label l5; Node n5;
           
        l1 = this.newLabel(1); n1 = this.newNode(l1);
        l2 = this.newLabel(2); n2 = this.newNode(l2);
        l3 = this.newLabel(3); n3 = this.newNode(l3);
        l4 = this.newLabel(4); n4 = this.newNode(l4);
        l5 = this.newLabel(5); n5 = this.newNode(l5);
                        
        bla = n1.setLeft(n2); bla = n1.setRight(n4);
        bla = n2.setLeft(n3); 
        bla = n3.setLeft(n1); bla = n3.setRight(n4);

        // in the beginning visited = false everywhere

        // search node 4 from 1
        bla = this.findPath (n1, n4); // expect YES
        bla = this.findPath (n4, n1); // expect NO
        bla = this.findPath (n3, n2); // expect YES
        bla = this.findPath (n4, n1); // expect NO
        bla = this.findPath (n3, n2); // expect YES
        bla = this.findPath (n4, n3); // expect NO
        bla = this.findPath (n5, n5); // expect YES
        bla = this.findPath (n5, n4); // expect NO

        return 0;
    }
}
