#include <runtime.h>

// POINTER OPERATIONS
int idris2_isNull(void *);
void *idris2_getNull();

// MEMORY OPERATIONS
extern char* TEXT_START;
extern char* TEXT_END;
extern char* DATA_START;
extern char* DATA_END;
extern char* RODATA_START;
extern char* RODATA_END;
extern char* BSS_START;
extern char* BSS_END;
extern char* KERNEL_STACK_START;
extern char* KERNEL_STACK_END;
extern char* MALLOC_START;
extern char* HEAP_START;
extern size_t HEAP_SIZE;

char* idris2_text_start();
char* idris2_text_end();
char* idris2_bss_start();
char* idris2_bss_end();
char* idris2_kernel_stack_start();
char* idris2_kernel_stack_end();
char* idris2_malloc_start();
char* idris2_heap_start();
size_t idris2_heap_size();

// Kernel init
Value *PrimIO_unsafePerformIO(Value * var_0);
Value *Main_kinit(Value * var_0);
size_t kinit();
void kinit_hart(size_t hartid);

// utils
void print(char *str);
char* itoa(int value, char* result, int base);
char* ptrtoa(void* ptr, char* result);

