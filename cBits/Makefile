CC := riscv64-unknown-elf-gcc 

TARGET = libidris_support

LIBTARGET = $(TARGET).a

IDRIS_LIB= $(shell pack data-path)/urefc

CFLAGS=-fno-builtin -Wno-implicit-function-declaration -Wno-pointer-to-int-cast -Wall -Wextra -c -mcmodel=medany -I$(IDRIS_LIB)
CBITSDIR = ./
CBITSSOURCES := $(shell find $(CBITSDIR) -name '*.c')
CBITSOBJS:= $(CBITSSOURCES:%.c=%.o)

all:
	$(MAKE) cBits
	$(MAKE) build

.PHONY: build
build: $(LIBTARGET)

cBits: $(CBITSOBJS)

cBits/%.o: %.c

$(LIBTARGET): $(CBITSOBJS)
	$(AR) rc $@ $^

clean:
	rm -f *.o *.a || true
