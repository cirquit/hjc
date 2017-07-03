// Conway' Game of Life
//
// For life forms, consult:
// http://www.bitstorm.org/gameoflife/lexicon/

class GameOfLife {
    public static void main(String[] a){
	System.out.println(new Game().run());
    }
}


class PrimeNumbers {
    int [] primes;
    int len;

    public int getPrime(int i) {
      return primes[i];
    }
    public int getCount() {
      return len;
    }

    public int gcd (int x, int y) {
	int res;
	if (x<y) {
	    res = this.gcd(y,x);
	} else if (y<1) {
	    res = x;
	} else {
	    res = this.gcd(y,x-y);
	}
	return res;
    }

    // isprime(n) = true   if n is prime relatively to all primes[0..len-1]
    public boolean isprime (int n) {
      int i;
      int p;
      boolean ok;   // is still prime?
      boolean cont; // continue loop
      ok   = true;
      cont = true;
      i = 0;
      while (ok && (i<len) && cont) {
	p = primes[i];
        ok = ok && this.gcd(p,n) < 2; // p and n have no common divisor > 1
	i=i+1;
        cont = p * p < n; // no need to continue if n greater sqrt(p)
      }
      return ok;
    }
	
    // compute the n first primes
    public int init (int n) {
	int i; 
        primes = new int [n];
        primes[0] = 2;  // set first prime to 2
        len = 1;       
	i = 3;          // test all odd numbers, starting with 3
	while (len < n) {
	    if (this.isprime(i)) {
	      primes[len]=i;
	      len=len+1;
	      i=i+2;
	    } else {
	      i=i+2;
	    }
	}
	return primes[len-1]; // return highest prime computed
    }

    public int print () {
      int i;
      i = 0;
      while (i < len) {
	System.out.println(primes[i]);
        i = i + 1;
      }
      return i;  
    }

    public int test(int n) {
      int bla;
      bla = this.init(n);
      return this.print();
    }

}

// compute Hash of a bit list using primes
class Hash {
  
  PrimeNumbers primes;

  // initialize by computing n primes
  public int init(int n) {
    primes = new PrimeNumbers();
    return primes.init(n);
  }

  public int hash(int[] arr) {
    int i;
    int ip;
    int sum;
    i = 0;   // index in arr
    ip = 0;  // index in primes array < primes.len
    sum = 0;
    while (i < arr.length) {
      if (0 < arr[i]) { // bit is set
        sum = sum + primes.getPrime(ip);
        i = i + 1;
      } else i = i + 1;  // bit is not set
      if (ip + 1 < primes.getCount()) ip = ip + 1;
      else ip = 0;
    }
    return sum;
  }

  public int test() {
    int[] arr;
    int bla;
    arr = new int[10];
    arr[1] = 1;
    arr[3] = 1;
    arr[5] = 1;
    arr[6] = 1;
    bla = this.init(10);
    return this.hash(arr); // should return 40
  }  

}



class Board {

  int [] board;
  int width;
  int height;

  // create empty board
  public int init(int w, int h) {
    width  = w;
    height = h;
    board  = new int[w*h];
    return w*h;
  }

  // create new board of same size
  public Board clone () {
    Board c;
    int bla;
    c = new Board();
    bla = c.init(width,height);
    return c;
  }

  // read field at (x,y)
  // if coordinates out of range, return 0
  public int at (int x, int y) {
    int value;
    if (0-1 < x && x < width && 0-1 < y && y < height)
      value = board[x + y * width];
    else value = 0;
    return value;
  } 

  // set field at (x,y)
  public int set (int x, int y, int value) {
    board[x + y * width] = value;
    return value;
  }

  // return whole board
  public int[] board() {
    return board;
  }

  // calculate board for previous board by the rules
  public int calc(Board pred) {
    int x;
    int y;
    int count;
    int bla;
    y = 0;
    while (y < height) {
      x = 0;
      while (x < width) {
        // count alive neighbors (all 8 directions)
        count = pred.at(x-1,y-1) + pred.at(x,y-1) + pred.at(x+1,y-1) 
              + pred.at(x-1,y)                    + pred.at(x+1,y) 
              + pred.at(x-1,y+1) + pred.at(x,y+1) + pred.at(x+1,y+1);
        // if point was dead, it becomes alive if exactly 3 neighbors
        // if point was alive it will stay alive with 2 or 3 neighbors
        if (2 - pred.at(x,y) < count && count < 4)  
          bla = this.set (x, y, 1); 
        else bla = this.set (x, y, 0);
        x = x + 1;
      }
      y = y + 1;
    }
    return 0;
  }

  // the simplest stator
  public int initStator () {
    int bla;
    bla = this.init (4,4);
    bla = this.set(1,1,1);
    bla = this.set(1,2,1);
    bla = this.set(2,1,1);
    bla = this.set(2,2,1);
    return 0;
  }

  // create life form 101
  public int init101 () {
    // !Name: 101
    // !
    // ....OO......OO....
    // ...O.O......O.O...
    // ...O..........O...
    // OO.O..........O.OO
    // OO.O.O..OO..O.O.OO
    // ...O.O.O..O.O.O...
    // ...O.O.O..O.O.O...
    // OO.O.O..OO..O.O.OO
    // OO.O..........O.OO
    // ...O..........O...
    // ...O.O......O.O...
    // ....OO......OO....
    // 
    width  = 18;
    height = 12;
    board  = new int[width*height];
    // ....OO......OO....
    board[0] = 0;
    board[1] = 0;
    board[2] = 0;
    board[3] = 0;
    board[4] = 1;
    board[5] = 1;
    board[6] = 0;
    board[7] = 0;
    board[8] = 0;
    board[9] = 0;
    board[10] = 0;
    board[11] = 0;
    board[12] = 1;
    board[13] = 1;
    board[14] = 0;
    board[15] = 0;
    board[16] = 0;
    board[17] = 0;
    // ...O.O......O.O...
    board[18] = 0;
    board[19] = 0;
    board[20] = 0;
    board[21] = 1;
    board[22] = 0;
    board[23] = 1;
    board[24] = 0;
    board[25] = 0;
    board[26] = 0;
    board[27] = 0;
    board[28] = 0;
    board[29] = 0;
    board[30] = 1;
    board[31] = 0;
    board[32] = 1;
    board[33] = 0;
    board[34] = 0;
    board[35] = 0;
    // ...O..........O...
    board[36] = 0;
    board[37] = 0;
    board[38] = 0;
    board[39] = 1;
    board[40] = 0;
    board[41] = 0;
    board[42] = 0;
    board[43] = 0;
    board[44] = 0;
    board[45] = 0;
    board[46] = 0;
    board[47] = 0;
    board[48] = 0;
    board[49] = 0;
    board[50] = 1;
    board[51] = 0;
    board[52] = 0;
    board[53] = 0;
    // OO.O..........O.OO
    board[54] = 1;
    board[55] = 1;
    board[56] = 0;
    board[57] = 1;
    board[58] = 0;
    board[59] = 0;
    board[60] = 0;
    board[61] = 0;
    board[62] = 0;
    board[63] = 0;
    board[64] = 0;
    board[65] = 0;
    board[66] = 0;
    board[67] = 0;
    board[68] = 1;
    board[69] = 0;
    board[70] = 1;
    board[71] = 1;
    // OO.O.O..OO..O.O.OO
    board[72] = 1;
    board[73] = 1;
    board[74] = 0;
    board[75] = 1;
    board[76] = 0;
    board[77] = 1;
    board[78] = 0;
    board[79] = 0;
    board[80] = 1;
    board[81] = 1;
    board[82] = 0;
    board[83] = 0;
    board[84] = 1;
    board[85] = 0;
    board[86] = 1;
    board[87] = 0;
    board[88] = 1;
    board[89] = 1;
    // ...O.O.O..O.O.O...
    board[90] = 0;
    board[91] = 0;
    board[92] = 0;
    board[93] = 1;
    board[94] = 0;
    board[95] = 1;
    board[96] = 0;
    board[97] = 1;
    board[98] = 0;
    board[99] = 0;
    board[100] = 1;
    board[101] = 0;
    board[102] = 1;
    board[103] = 0;
    board[104] = 1;
    board[105] = 0;
    board[106] = 0;
    board[107] = 0;
    // ...O.O.O..O.O.O...
    board[108] = 0;
    board[109] = 0;
    board[110] = 0;
    board[111] = 1;
    board[112] = 0;
    board[113] = 1;
    board[114] = 0;
    board[115] = 1;
    board[116] = 0;
    board[117] = 0;
    board[118] = 1;
    board[119] = 0;
    board[120] = 1;
    board[121] = 0;
    board[122] = 1;
    board[123] = 0;
    board[124] = 0;
    board[125] = 0;
    // OO.O.O..OO..O.O.OO
    board[126] = 1;
    board[127] = 1;
    board[128] = 0;
    board[129] = 1;
    board[130] = 0;
    board[131] = 1;
    board[132] = 0;
    board[133] = 0;
    board[134] = 1;
    board[135] = 1;
    board[136] = 0;
    board[137] = 0;
    board[138] = 1;
    board[139] = 0;
    board[140] = 1;
    board[141] = 0;
    board[142] = 1;
    board[143] = 1;
    // OO.O..........O.OO
    board[144] = 1;
    board[145] = 1;
    board[146] = 0;
    board[147] = 1;
    board[148] = 0;
    board[149] = 0;
    board[150] = 0;
    board[151] = 0;
    board[152] = 0;
    board[153] = 0;
    board[154] = 0;
    board[155] = 0;
    board[156] = 0;
    board[157] = 0;
    board[158] = 1;
    board[159] = 0;
    board[160] = 1;
    board[161] = 1;
    // ...O..........O...
    board[162] = 0;
    board[163] = 0;
    board[164] = 0;
    board[165] = 1;
    board[166] = 0;
    board[167] = 0;
    board[168] = 0;
    board[169] = 0;
    board[170] = 0;
    board[171] = 0;
    board[172] = 0;
    board[173] = 0;
    board[174] = 0;
    board[175] = 0;
    board[176] = 1;
    board[177] = 0;
    board[178] = 0;
    board[179] = 0;
    // ...O.O......O.O...
    board[180] = 0;
    board[181] = 0;
    board[182] = 0;
    board[183] = 1;
    board[184] = 0;
    board[185] = 1;
    board[186] = 0;
    board[187] = 0;
    board[188] = 0;
    board[189] = 0;
    board[190] = 0;
    board[191] = 0;
    board[192] = 1;
    board[193] = 0;
    board[194] = 1;
    board[195] = 0;
    board[196] = 0;
    board[197] = 0;
    // ....OO......OO....
    board[198] = 0;
    board[199] = 0;
    board[200] = 0;
    board[201] = 0;
    board[202] = 1;
    board[203] = 1;
    board[204] = 0;
    board[205] = 0;
    board[206] = 0;
    board[207] = 0;
    board[208] = 0;
    board[209] = 0;
    board[210] = 1;
    board[211] = 1;
    board[212] = 0;
    board[213] = 0;
    board[214] = 0;
    board[215] = 0;   
 
    return 0;
  }
  
  public int initGosperGliderGun0() {

    // ........................O...........
    // ......................O.O...........
    // ............OO......OO............OO
    // ...........O...O....OO............OO
    // OO........O.....O...OO..............
    // OO........O...O.OO....O.O...........
    // ..........O.....O.......O...........
    // ...........O...O....................
    // ............OO......................

    width  = 36;
    height = 9;
    board  = new int[width*height];
    // ........................O...........
    board[0] = 0;
    board[1] = 0;
    board[2] = 0;
    board[3] = 0;
    board[4] = 0;
    board[5] = 0;
    board[6] = 0;
    board[7] = 0;
    board[8] = 0;
    board[9] = 0;
    board[10] = 0;
    board[11] = 0;
    board[12] = 0;
    board[13] = 0;
    board[14] = 0;
    board[15] = 0;
    board[16] = 0;
    board[17] = 0;
    board[18] = 0;
    board[19] = 0;
    board[20] = 0;
    board[21] = 0;
    board[22] = 0;
    board[23] = 0;
    board[24] = 1;
    board[25] = 0;
    board[26] = 0;
    board[27] = 0;
    board[28] = 0;
    board[29] = 0;
    board[30] = 0;
    board[31] = 0;
    board[32] = 0;
    board[33] = 0;
    board[34] = 0;
    board[35] = 0;
    // ......................O.O...........
    board[36] = 0;
    board[37] = 0;
    board[38] = 0;
    board[39] = 0;
    board[40] = 0;
    board[41] = 0;
    board[42] = 0;
    board[43] = 0;
    board[44] = 0;
    board[45] = 0;
    board[46] = 0;
    board[47] = 0;
    board[48] = 0;
    board[49] = 0;
    board[50] = 0;
    board[51] = 0;
    board[52] = 0;
    board[53] = 0;
    board[54] = 0;
    board[55] = 0;
    board[56] = 0;
    board[57] = 0;
    board[58] = 1;
    board[59] = 0;
    board[60] = 1;
    board[61] = 0;
    board[62] = 0;
    board[63] = 0;
    board[64] = 0;
    board[65] = 0;
    board[66] = 0;
    board[67] = 0;
    board[68] = 0;
    board[69] = 0;
    board[70] = 0;
    board[71] = 0;
    // ............OO......OO............OO
    board[72] = 0;
    board[73] = 0;
    board[74] = 0;
    board[75] = 0;
    board[76] = 0;
    board[77] = 0;
    board[78] = 0;
    board[79] = 0;
    board[80] = 0;
    board[81] = 0;
    board[82] = 0;
    board[83] = 0;
    board[84] = 1;
    board[85] = 1;
    board[86] = 0;
    board[87] = 0;
    board[88] = 0;
    board[89] = 0;
    board[90] = 0;
    board[91] = 0;
    board[92] = 1;
    board[93] = 1;
    board[94] = 0;
    board[95] = 0;
    board[96] = 0;
    board[97] = 0;
    board[98] = 0;
    board[99] = 0;
    board[100] = 0;
    board[101] = 0;
    board[102] = 0;
    board[103] = 0;
    board[104] = 0;
    board[105] = 0;
    board[106] = 1;
    board[107] = 1;
    // ...........O...O....OO............OO
    board[108] = 0;
    board[109] = 0;
    board[110] = 0;
    board[111] = 0;
    board[112] = 0;
    board[113] = 0;
    board[114] = 0;
    board[115] = 0;
    board[116] = 0;
    board[117] = 0;
    board[118] = 0;
    board[119] = 1;
    board[120] = 0;
    board[121] = 0;
    board[122] = 0;
    board[123] = 1;
    board[124] = 0;
    board[125] = 0;
    board[126] = 0;
    board[127] = 0;
    board[128] = 1;
    board[129] = 1;
    board[130] = 0;
    board[131] = 0;
    board[132] = 0;
    board[133] = 0;
    board[134] = 0;
    board[135] = 0;
    board[136] = 0;
    board[137] = 0;
    board[138] = 0;
    board[139] = 0;
    board[140] = 0;
    board[141] = 0;
    board[142] = 1;
    board[143] = 1;
    // OO........O.....O...OO..............
    board[144] = 1;
    board[145] = 1;
    board[146] = 0;
    board[147] = 0;
    board[148] = 0;
    board[149] = 0;
    board[150] = 0;
    board[151] = 0;
    board[152] = 0;
    board[153] = 0;
    board[154] = 1;
    board[155] = 0;
    board[156] = 0;
    board[157] = 0;
    board[158] = 0;
    board[159] = 0;
    board[160] = 1;
    board[161] = 0;
    board[162] = 0;
    board[163] = 0;
    board[164] = 1;
    board[165] = 1;
    board[166] = 0;
    board[167] = 0;
    board[168] = 0;
    board[169] = 0;
    board[170] = 0;
    board[171] = 0;
    board[172] = 0;
    board[173] = 0;
    board[174] = 0;
    board[175] = 0;
    board[176] = 0;
    board[177] = 0;
    board[178] = 0;
    board[179] = 0;
    // OO........O...O.OO....O.O...........
    board[180] = 1;
    board[181] = 1;
    board[182] = 0;
    board[183] = 0;
    board[184] = 0;
    board[185] = 0;
    board[186] = 0;
    board[187] = 0;
    board[188] = 0;
    board[189] = 0;
    board[190] = 1;
    board[191] = 0;
    board[192] = 0;
    board[193] = 0;
    board[194] = 1;
    board[195] = 0;
    board[196] = 1;
    board[197] = 1;
    board[198] = 0;
    board[199] = 0;
    board[200] = 0;
    board[201] = 0;
    board[202] = 1;
    board[203] = 0;
    board[204] = 1;
    board[205] = 0;
    board[206] = 0;
    board[207] = 0;
    board[208] = 0;
    board[209] = 0;
    board[210] = 0;
    board[211] = 0;
    board[212] = 0;
    board[213] = 0;
    board[214] = 0;
    board[215] = 0;
    // ..........O.....O.......O...........
    board[216] = 0;
    board[217] = 0;
    board[218] = 0;
    board[219] = 0;
    board[220] = 0;
    board[221] = 0;
    board[222] = 0;
    board[223] = 0;
    board[224] = 0;
    board[225] = 0;
    board[226] = 1;
    board[227] = 0;
    board[228] = 0;
    board[229] = 0;
    board[230] = 0;
    board[231] = 0;
    board[232] = 1;
    board[233] = 0;
    board[234] = 0;
    board[235] = 0;
    board[236] = 0;
    board[237] = 0;
    board[238] = 0;
    board[239] = 0;
    board[240] = 1;
    board[241] = 0;
    board[242] = 0;
    board[243] = 0;
    board[244] = 0;
    board[245] = 0;
    board[246] = 0;
    board[247] = 0;
    board[248] = 0;
    board[249] = 0;
    board[250] = 0;
    board[251] = 0;
    // ...........O...O....................
    board[252] = 0;
    board[253] = 0;
    board[254] = 0;
    board[255] = 0;
    board[256] = 0;
    board[257] = 0;
    board[258] = 0;
    board[259] = 0;
    board[260] = 0;
    board[261] = 0;
    board[262] = 0;
    board[263] = 1;
    board[264] = 0;
    board[265] = 0;
    board[266] = 0;
    board[267] = 1;
    board[268] = 0;
    board[269] = 0;
    board[270] = 0;
    board[271] = 0;
    board[272] = 0;
    board[273] = 0;
    board[274] = 0;
    board[275] = 0;
    board[276] = 0;
    board[277] = 0;
    board[278] = 0;
    board[279] = 0;
    board[280] = 0;
    board[281] = 0;
    board[282] = 0;
    board[283] = 0;
    board[284] = 0;
    board[285] = 0;
    board[286] = 0;
    board[287] = 0;
    // ............OO......................
    board[288] = 0;
    board[289] = 0;
    board[290] = 0;
    board[291] = 0;
    board[292] = 0;
    board[293] = 0;
    board[294] = 0;
    board[295] = 0;
    board[296] = 0;
    board[297] = 0;
    board[298] = 0;
    board[299] = 0;
    board[300] = 1;
    board[301] = 1;
    board[302] = 0;
    board[303] = 0;
    board[304] = 0;
    board[305] = 0;
    board[306] = 0;
    board[307] = 0;
    board[308] = 0;
    board[309] = 0;
    board[310] = 0;
    board[311] = 0;
    board[312] = 0;
    board[313] = 0;
    board[314] = 0;
    board[315] = 0;
    board[316] = 0;
    board[317] = 0;
    board[318] = 0;
    board[319] = 0;
    board[320] = 0;
    board[321] = 0;
    board[322] = 0;
    board[323] = 0;
 
    return 0;
  }
  
  public int initGosperGliderGun() {

    // ...........................................
    // ...........................................
    // .........................O.................
    // .......................O.O.................
    // .............OO......OO............OO......
    // ............O...O....OO............OO......
    // .OO........O.....O...OO....................
    // .OO........O...O.OO....O.O.................
    // ...........O.....O.......O.................
    // ............O...O..........................
    // .............OO............................
    // ...........................................
    // ...........................................
    // ...........................................
    // ...........................................
    // ...........................................
    // ...........................................
    // ...........................................
    // ...........................................
    // ...........................................
    // ...........................................
    // ...........................................
    // ...........................................
    
    width  = 43;
    height = 23;
    board  = new int[width*height];
    // ...........................................
    board[0] = 0;
    board[1] = 0;
    board[2] = 0;
    board[3] = 0;
    board[4] = 0;
    board[5] = 0;
    board[6] = 0;
    board[7] = 0;
    board[8] = 0;
    board[9] = 0;
    board[10] = 0;
    board[11] = 0;
    board[12] = 0;
    board[13] = 0;
    board[14] = 0;
    board[15] = 0;
    board[16] = 0;
    board[17] = 0;
    board[18] = 0;
    board[19] = 0;
    board[20] = 0;
    board[21] = 0;
    board[22] = 0;
    board[23] = 0;
    board[24] = 0;
    board[25] = 0;
    board[26] = 0;
    board[27] = 0;
    board[28] = 0;
    board[29] = 0;
    board[30] = 0;
    board[31] = 0;
    board[32] = 0;
    board[33] = 0;
    board[34] = 0;
    board[35] = 0;
    board[36] = 0;
    board[37] = 0;
    board[38] = 0;
    board[39] = 0;
    board[40] = 0;
    board[41] = 0;
    board[42] = 0;
    // ...........................................
    board[43] = 0;
    board[44] = 0;
    board[45] = 0;
    board[46] = 0;
    board[47] = 0;
    board[48] = 0;
    board[49] = 0;
    board[50] = 0;
    board[51] = 0;
    board[52] = 0;
    board[53] = 0;
    board[54] = 0;
    board[55] = 0;
    board[56] = 0;
    board[57] = 0;
    board[58] = 0;
    board[59] = 0;
    board[60] = 0;
    board[61] = 0;
    board[62] = 0;
    board[63] = 0;
    board[64] = 0;
    board[65] = 0;
    board[66] = 0;
    board[67] = 0;
    board[68] = 0;
    board[69] = 0;
    board[70] = 0;
    board[71] = 0;
    board[72] = 0;
    board[73] = 0;
    board[74] = 0;
    board[75] = 0;
    board[76] = 0;
    board[77] = 0;
    board[78] = 0;
    board[79] = 0;
    board[80] = 0;
    board[81] = 0;
    board[82] = 0;
    board[83] = 0;
    board[84] = 0;
    board[85] = 0;
    // .........................O.................
    board[86] = 0;
    board[87] = 0;
    board[88] = 0;
    board[89] = 0;
    board[90] = 0;
    board[91] = 0;
    board[92] = 0;
    board[93] = 0;
    board[94] = 0;
    board[95] = 0;
    board[96] = 0;
    board[97] = 0;
    board[98] = 0;
    board[99] = 0;
    board[100] = 0;
    board[101] = 0;
    board[102] = 0;
    board[103] = 0;
    board[104] = 0;
    board[105] = 0;
    board[106] = 0;
    board[107] = 0;
    board[108] = 0;
    board[109] = 0;
    board[110] = 0;
    board[111] = 1;
    board[112] = 0;
    board[113] = 0;
    board[114] = 0;
    board[115] = 0;
    board[116] = 0;
    board[117] = 0;
    board[118] = 0;
    board[119] = 0;
    board[120] = 0;
    board[121] = 0;
    board[122] = 0;
    board[123] = 0;
    board[124] = 0;
    board[125] = 0;
    board[126] = 0;
    board[127] = 0;
    board[128] = 0;
    // .......................O.O.................
    board[129] = 0;
    board[130] = 0;
    board[131] = 0;
    board[132] = 0;
    board[133] = 0;
    board[134] = 0;
    board[135] = 0;
    board[136] = 0;
    board[137] = 0;
    board[138] = 0;
    board[139] = 0;
    board[140] = 0;
    board[141] = 0;
    board[142] = 0;
    board[143] = 0;
    board[144] = 0;
    board[145] = 0;
    board[146] = 0;
    board[147] = 0;
    board[148] = 0;
    board[149] = 0;
    board[150] = 0;
    board[151] = 0;
    board[152] = 1;
    board[153] = 0;
    board[154] = 1;
    board[155] = 0;
    board[156] = 0;
    board[157] = 0;
    board[158] = 0;
    board[159] = 0;
    board[160] = 0;
    board[161] = 0;
    board[162] = 0;
    board[163] = 0;
    board[164] = 0;
    board[165] = 0;
    board[166] = 0;
    board[167] = 0;
    board[168] = 0;
    board[169] = 0;
    board[170] = 0;
    board[171] = 0;
    // .............OO......OO............OO......
    board[172] = 0;
    board[173] = 0;
    board[174] = 0;
    board[175] = 0;
    board[176] = 0;
    board[177] = 0;
    board[178] = 0;
    board[179] = 0;
    board[180] = 0;
    board[181] = 0;
    board[182] = 0;
    board[183] = 0;
    board[184] = 0;
    board[185] = 1;
    board[186] = 1;
    board[187] = 0;
    board[188] = 0;
    board[189] = 0;
    board[190] = 0;
    board[191] = 0;
    board[192] = 0;
    board[193] = 1;
    board[194] = 1;
    board[195] = 0;
    board[196] = 0;
    board[197] = 0;
    board[198] = 0;
    board[199] = 0;
    board[200] = 0;
    board[201] = 0;
    board[202] = 0;
    board[203] = 0;
    board[204] = 0;
    board[205] = 0;
    board[206] = 0;
    board[207] = 1;
    board[208] = 1;
    board[209] = 0;
    board[210] = 0;
    board[211] = 0;
    board[212] = 0;
    board[213] = 0;
    board[214] = 0;
    // ............O...O....OO............OO......
    board[215] = 0;
    board[216] = 0;
    board[217] = 0;
    board[218] = 0;
    board[219] = 0;
    board[220] = 0;
    board[221] = 0;
    board[222] = 0;
    board[223] = 0;
    board[224] = 0;
    board[225] = 0;
    board[226] = 0;
    board[227] = 1;
    board[228] = 0;
    board[229] = 0;
    board[230] = 0;
    board[231] = 1;
    board[232] = 0;
    board[233] = 0;
    board[234] = 0;
    board[235] = 0;
    board[236] = 1;
    board[237] = 1;
    board[238] = 0;
    board[239] = 0;
    board[240] = 0;
    board[241] = 0;
    board[242] = 0;
    board[243] = 0;
    board[244] = 0;
    board[245] = 0;
    board[246] = 0;
    board[247] = 0;
    board[248] = 0;
    board[249] = 0;
    board[250] = 1;
    board[251] = 1;
    board[252] = 0;
    board[253] = 0;
    board[254] = 0;
    board[255] = 0;
    board[256] = 0;
    board[257] = 0;
    // .OO........O.....O...OO....................
    board[258] = 0;
    board[259] = 1;
    board[260] = 1;
    board[261] = 0;
    board[262] = 0;
    board[263] = 0;
    board[264] = 0;
    board[265] = 0;
    board[266] = 0;
    board[267] = 0;
    board[268] = 0;
    board[269] = 1;
    board[270] = 0;
    board[271] = 0;
    board[272] = 0;
    board[273] = 0;
    board[274] = 0;
    board[275] = 1;
    board[276] = 0;
    board[277] = 0;
    board[278] = 0;
    board[279] = 1;
    board[280] = 1;
    board[281] = 0;
    board[282] = 0;
    board[283] = 0;
    board[284] = 0;
    board[285] = 0;
    board[286] = 0;
    board[287] = 0;
    board[288] = 0;
    board[289] = 0;
    board[290] = 0;
    board[291] = 0;
    board[292] = 0;
    board[293] = 0;
    board[294] = 0;
    board[295] = 0;
    board[296] = 0;
    board[297] = 0;
    board[298] = 0;
    board[299] = 0;
    board[300] = 0;
    // .OO........O...O.OO....O.O.................
    board[301] = 0;
    board[302] = 1;
    board[303] = 1;
    board[304] = 0;
    board[305] = 0;
    board[306] = 0;
    board[307] = 0;
    board[308] = 0;
    board[309] = 0;
    board[310] = 0;
    board[311] = 0;
    board[312] = 1;
    board[313] = 0;
    board[314] = 0;
    board[315] = 0;
    board[316] = 1;
    board[317] = 0;
    board[318] = 1;
    board[319] = 1;
    board[320] = 0;
    board[321] = 0;
    board[322] = 0;
    board[323] = 0;
    board[324] = 1;
    board[325] = 0;
    board[326] = 1;
    board[327] = 0;
    board[328] = 0;
    board[329] = 0;
    board[330] = 0;
    board[331] = 0;
    board[332] = 0;
    board[333] = 0;
    board[334] = 0;
    board[335] = 0;
    board[336] = 0;
    board[337] = 0;
    board[338] = 0;
    board[339] = 0;
    board[340] = 0;
    board[341] = 0;
    board[342] = 0;
    board[343] = 0;
    // ...........O.....O.......O.................
    board[344] = 0;
    board[345] = 0;
    board[346] = 0;
    board[347] = 0;
    board[348] = 0;
    board[349] = 0;
    board[350] = 0;
    board[351] = 0;
    board[352] = 0;
    board[353] = 0;
    board[354] = 0;
    board[355] = 1;
    board[356] = 0;
    board[357] = 0;
    board[358] = 0;
    board[359] = 0;
    board[360] = 0;
    board[361] = 1;
    board[362] = 0;
    board[363] = 0;
    board[364] = 0;
    board[365] = 0;
    board[366] = 0;
    board[367] = 0;
    board[368] = 0;
    board[369] = 1;
    board[370] = 0;
    board[371] = 0;
    board[372] = 0;
    board[373] = 0;
    board[374] = 0;
    board[375] = 0;
    board[376] = 0;
    board[377] = 0;
    board[378] = 0;
    board[379] = 0;
    board[380] = 0;
    board[381] = 0;
    board[382] = 0;
    board[383] = 0;
    board[384] = 0;
    board[385] = 0;
    board[386] = 0;
    // ............O...O..........................
    board[387] = 0;
    board[388] = 0;
    board[389] = 0;
    board[390] = 0;
    board[391] = 0;
    board[392] = 0;
    board[393] = 0;
    board[394] = 0;
    board[395] = 0;
    board[396] = 0;
    board[397] = 0;
    board[398] = 0;
    board[399] = 1;
    board[400] = 0;
    board[401] = 0;
    board[402] = 0;
    board[403] = 1;
    board[404] = 0;
    board[405] = 0;
    board[406] = 0;
    board[407] = 0;
    board[408] = 0;
    board[409] = 0;
    board[410] = 0;
    board[411] = 0;
    board[412] = 0;
    board[413] = 0;
    board[414] = 0;
    board[415] = 0;
    board[416] = 0;
    board[417] = 0;
    board[418] = 0;
    board[419] = 0;
    board[420] = 0;
    board[421] = 0;
    board[422] = 0;
    board[423] = 0;
    board[424] = 0;
    board[425] = 0;
    board[426] = 0;
    board[427] = 0;
    board[428] = 0;
    board[429] = 0;
    // .............OO............................
    board[430] = 0;
    board[431] = 0;
    board[432] = 0;
    board[433] = 0;
    board[434] = 0;
    board[435] = 0;
    board[436] = 0;
    board[437] = 0;
    board[438] = 0;
    board[439] = 0;
    board[440] = 0;
    board[441] = 0;
    board[442] = 0;
    board[443] = 1;
    board[444] = 1;
    board[445] = 0;
    board[446] = 0;
    board[447] = 0;
    board[448] = 0;
    board[449] = 0;
    board[450] = 0;
    board[451] = 0;
    board[452] = 0;
    board[453] = 0;
    board[454] = 0;
    board[455] = 0;
    board[456] = 0;
    board[457] = 0;
    board[458] = 0;
    board[459] = 0;
    board[460] = 0;
    board[461] = 0;
    board[462] = 0;
    board[463] = 0;
    board[464] = 0;
    board[465] = 0;
    board[466] = 0;
    board[467] = 0;
    board[468] = 0;
    board[469] = 0;
    board[470] = 0;
    board[471] = 0;
    board[472] = 0;
    // ...........................................
    board[473] = 0;
    board[474] = 0;
    board[475] = 0;
    board[476] = 0;
    board[477] = 0;
    board[478] = 0;
    board[479] = 0;
    board[480] = 0;
    board[481] = 0;
    board[482] = 0;
    board[483] = 0;
    board[484] = 0;
    board[485] = 0;
    board[486] = 0;
    board[487] = 0;
    board[488] = 0;
    board[489] = 0;
    board[490] = 0;
    board[491] = 0;
    board[492] = 0;
    board[493] = 0;
    board[494] = 0;
    board[495] = 0;
    board[496] = 0;
    board[497] = 0;
    board[498] = 0;
    board[499] = 0;
    board[500] = 0;
    board[501] = 0;
    board[502] = 0;
    board[503] = 0;
    board[504] = 0;
    board[505] = 0;
    board[506] = 0;
    board[507] = 0;
    board[508] = 0;
    board[509] = 0;
    board[510] = 0;
    board[511] = 0;
    board[512] = 0;
    board[513] = 0;
    board[514] = 0;
    board[515] = 0;
    // ...........................................
    board[516] = 0;
    board[517] = 0;
    board[518] = 0;
    board[519] = 0;
    board[520] = 0;
    board[521] = 0;
    board[522] = 0;
    board[523] = 0;
    board[524] = 0;
    board[525] = 0;
    board[526] = 0;
    board[527] = 0;
    board[528] = 0;
    board[529] = 0;
    board[530] = 0;
    board[531] = 0;
    board[532] = 0;
    board[533] = 0;
    board[534] = 0;
    board[535] = 0;
    board[536] = 0;
    board[537] = 0;
    board[538] = 0;
    board[539] = 0;
    board[540] = 0;
    board[541] = 0;
    board[542] = 0;
    board[543] = 0;
    board[544] = 0;
    board[545] = 0;
    board[546] = 0;
    board[547] = 0;
    board[548] = 0;
    board[549] = 0;
    board[550] = 0;
    board[551] = 0;
    board[552] = 0;
    board[553] = 0;
    board[554] = 0;
    board[555] = 0;
    board[556] = 0;
    board[557] = 0;
    board[558] = 0;
    // ...........................................
    board[559] = 0;
    board[560] = 0;
    board[561] = 0;
    board[562] = 0;
    board[563] = 0;
    board[564] = 0;
    board[565] = 0;
    board[566] = 0;
    board[567] = 0;
    board[568] = 0;
    board[569] = 0;
    board[570] = 0;
    board[571] = 0;
    board[572] = 0;
    board[573] = 0;
    board[574] = 0;
    board[575] = 0;
    board[576] = 0;
    board[577] = 0;
    board[578] = 0;
    board[579] = 0;
    board[580] = 0;
    board[581] = 0;
    board[582] = 0;
    board[583] = 0;
    board[584] = 0;
    board[585] = 0;
    board[586] = 0;
    board[587] = 0;
    board[588] = 0;
    board[589] = 0;
    board[590] = 0;
    board[591] = 0;
    board[592] = 0;
    board[593] = 0;
    board[594] = 0;
    board[595] = 0;
    board[596] = 0;
    board[597] = 0;
    board[598] = 0;
    board[599] = 0;
    board[600] = 0;
    board[601] = 0;
    // ...........................................
    board[602] = 0;
    board[603] = 0;
    board[604] = 0;
    board[605] = 0;
    board[606] = 0;
    board[607] = 0;
    board[608] = 0;
    board[609] = 0;
    board[610] = 0;
    board[611] = 0;
    board[612] = 0;
    board[613] = 0;
    board[614] = 0;
    board[615] = 0;
    board[616] = 0;
    board[617] = 0;
    board[618] = 0;
    board[619] = 0;
    board[620] = 0;
    board[621] = 0;
    board[622] = 0;
    board[623] = 0;
    board[624] = 0;
    board[625] = 0;
    board[626] = 0;
    board[627] = 0;
    board[628] = 0;
    board[629] = 0;
    board[630] = 0;
    board[631] = 0;
    board[632] = 0;
    board[633] = 0;
    board[634] = 0;
    board[635] = 0;
    board[636] = 0;
    board[637] = 0;
    board[638] = 0;
    board[639] = 0;
    board[640] = 0;
    board[641] = 0;
    board[642] = 0;
    board[643] = 0;
    board[644] = 0;
    // ...........................................
    board[645] = 0;
    board[646] = 0;
    board[647] = 0;
    board[648] = 0;
    board[649] = 0;
    board[650] = 0;
    board[651] = 0;
    board[652] = 0;
    board[653] = 0;
    board[654] = 0;
    board[655] = 0;
    board[656] = 0;
    board[657] = 0;
    board[658] = 0;
    board[659] = 0;
    board[660] = 0;
    board[661] = 0;
    board[662] = 0;
    board[663] = 0;
    board[664] = 0;
    board[665] = 0;
    board[666] = 0;
    board[667] = 0;
    board[668] = 0;
    board[669] = 0;
    board[670] = 0;
    board[671] = 0;
    board[672] = 0;
    board[673] = 0;
    board[674] = 0;
    board[675] = 0;
    board[676] = 0;
    board[677] = 0;
    board[678] = 0;
    board[679] = 0;
    board[680] = 0;
    board[681] = 0;
    board[682] = 0;
    board[683] = 0;
    board[684] = 0;
    board[685] = 0;
    board[686] = 0;
    board[687] = 0;
    // ...........................................
    board[688] = 0;
    board[689] = 0;
    board[690] = 0;
    board[691] = 0;
    board[692] = 0;
    board[693] = 0;
    board[694] = 0;
    board[695] = 0;
    board[696] = 0;
    board[697] = 0;
    board[698] = 0;
    board[699] = 0;
    board[700] = 0;
    board[701] = 0;
    board[702] = 0;
    board[703] = 0;
    board[704] = 0;
    board[705] = 0;
    board[706] = 0;
    board[707] = 0;
    board[708] = 0;
    board[709] = 0;
    board[710] = 0;
    board[711] = 0;
    board[712] = 0;
    board[713] = 0;
    board[714] = 0;
    board[715] = 0;
    board[716] = 0;
    board[717] = 0;
    board[718] = 0;
    board[719] = 0;
    board[720] = 0;
    board[721] = 0;
    board[722] = 0;
    board[723] = 0;
    board[724] = 0;
    board[725] = 0;
    board[726] = 0;
    board[727] = 0;
    board[728] = 0;
    board[729] = 0;
    board[730] = 0;
    // ...........................................
    board[731] = 0;
    board[732] = 0;
    board[733] = 0;
    board[734] = 0;
    board[735] = 0;
    board[736] = 0;
    board[737] = 0;
    board[738] = 0;
    board[739] = 0;
    board[740] = 0;
    board[741] = 0;
    board[742] = 0;
    board[743] = 0;
    board[744] = 0;
    board[745] = 0;
    board[746] = 0;
    board[747] = 0;
    board[748] = 0;
    board[749] = 0;
    board[750] = 0;
    board[751] = 0;
    board[752] = 0;
    board[753] = 0;
    board[754] = 0;
    board[755] = 0;
    board[756] = 0;
    board[757] = 0;
    board[758] = 0;
    board[759] = 0;
    board[760] = 0;
    board[761] = 0;
    board[762] = 0;
    board[763] = 0;
    board[764] = 0;
    board[765] = 0;
    board[766] = 0;
    board[767] = 0;
    board[768] = 0;
    board[769] = 0;
    board[770] = 0;
    board[771] = 0;
    board[772] = 0;
    board[773] = 0;
    // ...........................................
    board[774] = 0;
    board[775] = 0;
    board[776] = 0;
    board[777] = 0;
    board[778] = 0;
    board[779] = 0;
    board[780] = 0;
    board[781] = 0;
    board[782] = 0;
    board[783] = 0;
    board[784] = 0;
    board[785] = 0;
    board[786] = 0;
    board[787] = 0;
    board[788] = 0;
    board[789] = 0;
    board[790] = 0;
    board[791] = 0;
    board[792] = 0;
    board[793] = 0;
    board[794] = 0;
    board[795] = 0;
    board[796] = 0;
    board[797] = 0;
    board[798] = 0;
    board[799] = 0;
    board[800] = 0;
    board[801] = 0;
    board[802] = 0;
    board[803] = 0;
    board[804] = 0;
    board[805] = 0;
    board[806] = 0;
    board[807] = 0;
    board[808] = 0;
    board[809] = 0;
    board[810] = 0;
    board[811] = 0;
    board[812] = 0;
    board[813] = 0;
    board[814] = 0;
    board[815] = 0;
    board[816] = 0;
    // ...........................................
    board[817] = 0;
    board[818] = 0;
    board[819] = 0;
    board[820] = 0;
    board[821] = 0;
    board[822] = 0;
    board[823] = 0;
    board[824] = 0;
    board[825] = 0;
    board[826] = 0;
    board[827] = 0;
    board[828] = 0;
    board[829] = 0;
    board[830] = 0;
    board[831] = 0;
    board[832] = 0;
    board[833] = 0;
    board[834] = 0;
    board[835] = 0;
    board[836] = 0;
    board[837] = 0;
    board[838] = 0;
    board[839] = 0;
    board[840] = 0;
    board[841] = 0;
    board[842] = 0;
    board[843] = 0;
    board[844] = 0;
    board[845] = 0;
    board[846] = 0;
    board[847] = 0;
    board[848] = 0;
    board[849] = 0;
    board[850] = 0;
    board[851] = 0;
    board[852] = 0;
    board[853] = 0;
    board[854] = 0;
    board[855] = 0;
    board[856] = 0;
    board[857] = 0;
    board[858] = 0;
    board[859] = 0;
    // ...........................................
    board[860] = 0;
    board[861] = 0;
    board[862] = 0;
    board[863] = 0;
    board[864] = 0;
    board[865] = 0;
    board[866] = 0;
    board[867] = 0;
    board[868] = 0;
    board[869] = 0;
    board[870] = 0;
    board[871] = 0;
    board[872] = 0;
    board[873] = 0;
    board[874] = 0;
    board[875] = 0;
    board[876] = 0;
    board[877] = 0;
    board[878] = 0;
    board[879] = 0;
    board[880] = 0;
    board[881] = 0;
    board[882] = 0;
    board[883] = 0;
    board[884] = 0;
    board[885] = 0;
    board[886] = 0;
    board[887] = 0;
    board[888] = 0;
    board[889] = 0;
    board[890] = 0;
    board[891] = 0;
    board[892] = 0;
    board[893] = 0;
    board[894] = 0;
    board[895] = 0;
    board[896] = 0;
    board[897] = 0;
    board[898] = 0;
    board[899] = 0;
    board[900] = 0;
    board[901] = 0;
    board[902] = 0;
    // ...........................................
    board[903] = 0;
    board[904] = 0;
    board[905] = 0;
    board[906] = 0;
    board[907] = 0;
    board[908] = 0;
    board[909] = 0;
    board[910] = 0;
    board[911] = 0;
    board[912] = 0;
    board[913] = 0;
    board[914] = 0;
    board[915] = 0;
    board[916] = 0;
    board[917] = 0;
    board[918] = 0;
    board[919] = 0;
    board[920] = 0;
    board[921] = 0;
    board[922] = 0;
    board[923] = 0;
    board[924] = 0;
    board[925] = 0;
    board[926] = 0;
    board[927] = 0;
    board[928] = 0;
    board[929] = 0;
    board[930] = 0;
    board[931] = 0;
    board[932] = 0;
    board[933] = 0;
    board[934] = 0;
    board[935] = 0;
    board[936] = 0;
    board[937] = 0;
    board[938] = 0;
    board[939] = 0;
    board[940] = 0;
    board[941] = 0;
    board[942] = 0;
    board[943] = 0;
    board[944] = 0;
    board[945] = 0;
    // ...........................................
    board[946] = 0;
    board[947] = 0;
    board[948] = 0;
    board[949] = 0;
    board[950] = 0;
    board[951] = 0;
    board[952] = 0;
    board[953] = 0;
    board[954] = 0;
    board[955] = 0;
    board[956] = 0;
    board[957] = 0;
    board[958] = 0;
    board[959] = 0;
    board[960] = 0;
    board[961] = 0;
    board[962] = 0;
    board[963] = 0;
    board[964] = 0;
    board[965] = 0;
    board[966] = 0;
    board[967] = 0;
    board[968] = 0;
    board[969] = 0;
    board[970] = 0;
    board[971] = 0;
    board[972] = 0;
    board[973] = 0;
    board[974] = 0;
    board[975] = 0;
    board[976] = 0;
    board[977] = 0;
    board[978] = 0;
    board[979] = 0;
    board[980] = 0;
    board[981] = 0;
    board[982] = 0;
    board[983] = 0;
    board[984] = 0;
    board[985] = 0;
    board[986] = 0;
    board[987] = 0;
    board[988] = 0;
        
    return 0;
  }
}

class Game {

  // run game for n steps
  public int run() {

    Hash hash;
    Board bold;
    Board b;
    Board bnew;

    int bla;
    int n;
    int i;

    bold = new Board();

    // change initial configuration and number of steps here

//    n = 10;
//    bla = bold.initStator();

//    n = 100;
//    bla = bold.init101();

    n = 200;
    bla = bold.initGosperGliderGun();

    hash = new Hash();
    bla  = hash.init(bold.board().length);

    bnew = bold.clone();
    i = 0;
    while (i < n) {

      System.out.println(hash.hash(bold.board()));  // show old board
      bla = bnew.calc(bold);    // calculate new board

      // swap boards      
      b = bold;
      bold = bnew;
      bnew = b;

      i = i + 1;
    }    
    return 999999999;
  }
}
