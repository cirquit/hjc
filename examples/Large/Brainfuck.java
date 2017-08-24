/* Brainfuck-Interpreter mit Beispielprogramm
 * prg = "+++++++++++++++++++++++++++++++++>+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++>++++++++++>+++++++>>+<<[>++++++++++++++++++++++++++++++++++++++++++++++++.------------------------------------------------<<<<.-.>.<.+>>>>>>>++++++++++<<[->+>-[>+>>]>[+[-<+>]>+>>]<<<<<<]>[<+>-]>[-]>>>++++++++++<[->-[>+>>]>[+[-<+>]>+>>]<<<<<]>[-]>>[++++++++++++++++++++++++++++++++++++++++++++++++.[-]]<[++++++++++++++++++++++++++++++++++++++++++++++++.[-]]<<<++++++++++++++++++++++++++++++++++++++++++++++++.[-]<<<<<<.>>+>[>>+<<-]>>[<<<[>+>+<<-]>>[<<+>>-]>-]<<<<-]"
 *
 * Programme können so kodiert werden:
 *  cmdnr c = Data.List.findIndex (==c) "><+-.,[]"
 *  prg' = Data.Maybe.catMaybes $ map cmdnr prg
 *  main = mapM putStr ["prg[" ++ (show i) ++ "] = " ++ show c ++ ";\n" | (c,i) <-  zip prg' [0..]]
 * Das Beispielprogramm ist noch binaer gepackt.
*/
class Brainfuck {

  public static void main(String[] args) {
    System.out.print((char)(new BF()).init().run());
  }
}

class Stack {

  int[] elements;
  int top;

  public Stack init(int size) {
    elements = new int[size];
    top = 0;
    return this;
  }

  public int top() {
    return elements[top-1];
  }

  public int push(int i) {
    elements[top] = i;
    top = top+1;
    return i;
  }

  public int pop() {
    top = top - 1;
    return elements[top];
  }
}

class BF {


  int[] cells;

  int[] prg;
  int ip;
  Stack stack;

  int ptr;

  public BF init() {
    int[] packedprg;
    int i;
    int j;
    int k;

    cells = new int[30000];
    stack = new Stack().init(1000);
    ip = 0;

    packedprg = new int[56];
    packedprg[0] = 306783378;
    packedprg[1] = 306783378;
    packedprg[2] = 306783378;
    packedprg[3] = 306259090;
    packedprg[4] = 306783378;
    packedprg[5] = 306783378;
    packedprg[6] = 306783378;
    packedprg[7] = 306783378;
    packedprg[8] = 306783378;
    packedprg[9] = 306775186;
    packedprg[10] = 306782354;
    packedprg[11] = 306709582;
    packedprg[12] = 38347922;
    packedprg[13] = 306783378;
    packedprg[14] = 306783378;
    packedprg[15] = 306783378;
    packedprg[16] = 306783380;
    packedprg[17] = 460175067;
    packedprg[18] = 460175067;
    packedprg[19] = 460175067;
    packedprg[20] = 460175067;
    packedprg[21] = 460175049;
    packedprg[22] = 160303202;
    packedprg[23] = 146;
    packedprg[24] = 306783310;
    packedprg[25] = 406970496;
    packedprg[26] = 952840839;
    packedprg[27] = 33788489;
    packedprg[28] = 253272312;
    packedprg[29] = 870319250;
    packedprg[30] = 306783128;
    packedprg[31] = 503844402;
    packedprg[32] = 858288256;
    packedprg[33] = 958701107;
    packedprg[34] = 941171858;
    packedprg[35] = 306783378;
    packedprg[36] = 306783378;
    packedprg[37] = 306783378;
    packedprg[38] = 306783378;
    packedprg[39] = 312082034;
    packedprg[40] = 306783378;
    packedprg[41] = 306783378;
    packedprg[42] = 306783378;
    packedprg[43] = 306783378;
    packedprg[44] = 306783539;
    packedprg[45] = 1059366034;
    packedprg[46] = 306783378;
    packedprg[47] = 306783378;
    packedprg[48] = 306783378;
    packedprg[49] = 306783378;
    packedprg[50] = 307445321;
    packedprg[51] = 154141744;
    packedprg[52] = 36040753;
    packedprg[53] = 163644491;
    packedprg[54] = 941134851;
    packedprg[55] = 947688031;

    prg = new int[560];
    i = 0;
    j = 0;
    while (i < 560) {
      k = 134217728;
      while (0 < k) {
        prg[i] = packedprg[j] / k;
        packedprg[j] = packedprg[j] - k*prg[i];
        k = k / 8;
        i = i + 1;
      }
      j = j + 1;
    }

    return this;
  }

  public boolean eq(int x, int y) {
    return (x < y+1) && (y < x+1);
  }

  public int run() {
    int level;

    while (ip < prg.length) {
      if (this.eq(prg[ip], 0)) {
        ptr = ptr + 1;
      } else if (this.eq(prg[ip], 1)) {
        ptr = ptr - 1;
      } else if (this.eq(prg[ip], 2)) {
        cells[ptr] = cells[ptr] + 1;
      } else if (this.eq(prg[ip], 3)) {
        cells[ptr] = cells[ptr] - 1;
      } else if (this.eq(prg[ip], 4)) {
        System.out.print((char)cells[ptr]);
      } else if (this.eq(prg[ip], 5)) {
        System.out.println(0-255);
      } else if (this.eq(prg[ip], 6)) {
        if (this.eq(cells[ptr], 0)) {
          level = 1;
          ip = ip + 1;
          while (0 < level && ip < prg.length) {
            if (this.eq(prg[ip], 6)) {
              level = level + 1;
            } else if (this.eq(prg[ip], 7)) {
              level = level - 1;
            } else {}
            ip = ip + 1;
          }
          ip = ip - 1;
        } else {
          ip = stack.push(ip);
        }
      } else {
        ip = stack.pop();
        ip = ip - 1;
      }
      ip = ip + 1;
    }
    return 13;
  }
}
