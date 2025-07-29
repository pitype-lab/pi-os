#!/bin/bash
qemu-system-riscv64 -machine virt -cpu rv64 -d in_asm,mmu -D mmu.log -smp 4 -m 128M -nographic -bios none -kernel ./kernel.elf -serial mon:stdio
