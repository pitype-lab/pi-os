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
char* idris2_text_start() { return TEXT_START; }
char* idris2_text_end() { return TEXT_END; }
char* idris2_data_start() { return DATA_START; }
char* idris2_data_end() { return DATA_END; }
char* idris2_rodata_start() { return RODATA_START; }
char* idris2_rodata_end() { return RODATA_END; }
char* idris2_bss_start() { return BSS_START; }
char* idris2_bss_end() { return BSS_END; }
char* idris2_kernel_stack_start() { return KERNEL_STACK_START; }
char* idris2_kernel_stack_end() { return KERNEL_STACK_END; }
char* idris2_malloc_start() { return MALLOC_START; }
char* idris2_heap_start() { return HEAP_START; }
size_t idris2_heap_size() { return HEAP_SIZE; }

// Pointer to net device state page (heap-allocated, single 8-byte aligned global)
// Layout: offset 0 = base (u64), offset 8 = avail addr (u64), offset 16 = avail idx (u16)
static uint64_t net_state_addr = 0;
uint64_t net_state_get_addr()                 { return net_state_addr; }
void     net_state_set_addr(uint64_t addr)    { net_state_addr = addr; }

Value *Trap_m_trap(Value *, Value *, Value *, Value *, Value *);
Value *PrimIO_unsafePerformIO(Value *);

uint64_t m_trap(size_t epc, size_t tval, size_t cause, size_t hart, size_t status) {
  Value *unsafeIO = (Value *)idris2_mkClosure((Value *(*)())PrimIO_unsafePerformIO, 1, 1);

  Value *var_epc    = idris2_mkBits64(epc);
  Value *var_tval   = idris2_mkBits64(tval);
  Value *var_cause  = idris2_mkBits64(cause);
  Value *var_hart   = idris2_mkBits64(hart);
  Value *var_status = idris2_mkBits64(status);

  Value *ioAction = idris2_trampoline(Trap_m_trap(var_epc, var_tval, var_cause, var_hart, var_status));
  Value *result = idris2_apply_closure(ioAction, idris2_newReference(unsafeIO));

  return (uint64_t)idris2_vp_to_Bits64(result);
}

// Virtio net device state (shared between setupNetwork and IRQ handler)
uint64_t net_dev_base = 0;
uint64_t net_dev_avail = 0;
uint16_t net_dev_avail_idx = 0;

/*void net_dev_set(uint64_t base, uint64_t avail, uint16_t idx) {
    net_dev_base = base;
    net_dev_avail = avail;
    net_dev_avail_idx = idx;
}

uint64_t net_dev_get_base()  { return net_dev_base; }
uint64_t net_dev_get_avail() { return net_dev_avail; }
uint16_t net_dev_get_avail_idx() { return net_dev_avail_idx; }
void     net_dev_set_avail_idx(uint16_t idx) { net_dev_avail_idx = idx; } */

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

void u64_to_str(uint64_t value, char *str) {
    char buffer[21];  // Max length for 64-bit decimal + null terminator
    int i = 0;

    if (value == 0) {
        str[0] = '0';
        str[1] = '\0';
        return;
    }

    while (value > 0) {
        buffer[i++] = '0' + (value % 10);
        value /= 10;
    }

    // Reverse the digits into the output string
    for (int j = 0; j < i; ++j) {
        str[j] = buffer[i - j - 1];
    }
    str[i] = '\0';
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
