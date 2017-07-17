#include <stdint.h>
#define MEM(x) *((int32_t*)(x))

int32_t L_halloc(int32_t size);
int32_t L_println_int(int32_t n);
int32_t L_print_char(int32_t n);
int32_t L_raise(int32_t rc);
int32_t Lmain () ;

int32_t Lmain () {
int32_t t1, t2;
/* MOVE(TEMP(t1), CALL(NAME(L_println_int), BINOP(PLUS, CONST(1), CONST(2)))) */
int32_t t0 = L_println_int((1 + 2));
t1 = t0;
/* MOVE(TEMP(t2), CONST(0)) */
t2 = 0;
return t2;
}

