class Factorial{
    public static void main(String[] a){
	System.out.println(new Fac().ComputeFac(10,new int[10],9));
    }
}

class Fac {

    int num;
    int[] num;

    public int ComputeFac(int num, int[] num, int num){
        int[] num_aux;
	int num_aux ;
	if (num < 1)
	    num_aux = 1 ;
	else 
	    num_aux = num * (this.ComputeFac(num-1,num_aux,num)) ;
	return num_aux ;
    }

}
