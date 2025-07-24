IDRIS_LIB= $(shell pack data-path)/urefc
IDRIS_SUPPORT=cBits

CC := riscv64-unknown-elf-gcc
CFLAGS=-Wno-int-conversion -Wno-implicit-function-declaration -fno-builtin -c -mcmodel=medany -I$(IDRIS_LIB) -I$(IDRIS_SUPPORT)

BOOTDIR = boot
BOOTSOURCES := $(shell find $(BOOTDIR) -name '*.s')
BOOTOBJS:= $(BOOTSOURCES:%.s=%.o)

all:
	$(MAKE) -C boot/
	pack install-deps
	$(MAKE) -C cBits/
	IDRIS2_CC=$(CC) IDRIS2_CFLAGS="$(CFLAGS)" pack build pi.ipkg
	riscv64-unknown-elf-ld -T lds/virt.lds -L$(IDRIS_LIB) -L$(IDRIS_SUPPORT) -L$(shell pack libs-path | tr ':' '\n' | grep '/cptr/') -nostdlib build/exec/kernel.o  $(BOOTOBJS) --whole-archive -lidris2_urefc -lcptr-idris -lidris_support -o kernel.elf
 
kernel:
	$(CC) $(CFLAGS) build/exec/kernel.c -c -o build/exec/kernel.o

watch:
	find ./ -iname "*.idr" | entr -sndc 'make && ./boot.bash'

clean:
	$(MAKE) clean -C boot
	$(MAKE) clean -C cBits
	rm -rf build/
	rm -f *.o *.elf *.c || true
