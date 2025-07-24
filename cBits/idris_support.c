#include "idris_support.h"

// POINTER OPERATIONS

int idris2_isNull(void *ptr) { return (ptr == NULL); }
void *idris2_getNull() { return NULL; }

Value* idris2_anyptr_nat(void *p) {
	Value_Integer *retVal = idris2_mkInteger();
	mpz_set_si(retVal->i,(uintptr_t)p);

	return (Value *)retVal;
}

// MEMORY OPERATIONS

size_t idris2_heap_size() { return HEAP_SIZE; }
char* idris2_heap_start() { return HEAP_START; }

// Kernel init
size_t kinit() {
 Value *closure_0 = (Value *)idris2_mkClosure((Value *(*)())Main_kinit, 1, 1);
                                                            
  Value * var_0 = closure_0;                               
  Value *closure_1 = (Value *)idris2_mkClosure((Value *(*)())PrimIO_unsafePerformIO, 1, 1);
  ((Value_Closure*)closure_1)->args[0] = var_0;

	Value_Integer *ret = (Value_Integer *)idris2_trampoline(closure_0);

  return (size_t)mpz_get_lsb(ret->i,32);
}

// utils

char *UART=(char*) 0x10000000;

void putChar(char c) {
	*UART=c;
}

void print(char *str) {
	while(*str!='\0') {
		putChar(*str);
		str++;
	}

	putChar('\n');
}

char* itoa(int value, char* result, int base) {
    // check that the base if valid
    if (base < 2 || base > 36) { *result = '\0'; return result; }

    char* ptr = result, *ptr1 = result, tmp_char;
    int tmp_value;

    do {
        tmp_value = value;
        value /= base;
        *ptr++ = "zyxwvutsrqponmlkjihgfedcba9876543210123456789abcdefghijklmnopqrstuvwxyz" [35 + (tmp_value - value * base)];
    } while ( value );

    // Apply negative sign
    if (tmp_value < 0) *ptr++ = '-';
    *ptr-- = '\0';

    // Reverse the string
    while(ptr1 < ptr) {
        tmp_char = *ptr;
        *ptr--= *ptr1;
        *ptr1++ = tmp_char;
    }
    return result;
}

char* ptrtoa(void* ptr, char* result) {
    uintptr_t value = (uintptr_t)ptr;  // Convertit le pointeur en entier non signé
    char* ptr1 = result;
    char* ptr2;
    char tmp_char;

    // On commence par écrire "0x"
    *result++ = '0';
    *result++ = 'x';

    // Aucune valeur spéciale pour un pointeur nul ici
    if (value == 0) {
        *result++ = '0';
        *result = '\0';
        return ptr1;
    }

    ptr2 = result;

    // Conversion en base 16 (hexadécimal)
    while (value > 0) {
        int digit = value % 16;
        *result++ = "0123456789abcdef"[digit];
        value /= 16;
    }

    *result = '\0';
    result--;

    // Inverser les chiffres hexadécimaux
    while (ptr2 < result) {
        tmp_char = *result;
        *result-- = *ptr2;
        *ptr2++ = tmp_char;
    }

    return ptr1;
}
