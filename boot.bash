#!/bin/bash
qemu-system-riscv64 -machine virt -cpu rv64 -smp 4 -m 128M -nographic -bios none -kernel ./kernel.elf -serial mon:stdio
