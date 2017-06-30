class Factorial{
    public static void main(String[] a){
	System.out.println(new Fac().ComputeFac(10));
    }
}

class Fac {

    public int ComputeFac(int num){
	int num_aux;
//	if (num == 1 && false)
//	    num_aux = 1 ;
//	else 
//	    num_aux = num * (this.ComputeFac(num-1)) ;
//	return num_aux ;

    num_aux = 0;
    while (num_aux < 100) {
        num_aux = num_aux + num;
    }

    return num_aux;
    }

}
