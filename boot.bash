#!/bin/bash
qemu-system-riscv64 \
  -machine virt \
  -nographic \
  -bios none \
  -kernel ./kernel.elf \
  -serial mon:stdio \
  -device virtio-net-device,netdev=net0 \
  -netdev user,id=net0,hostfwd=tcp::8080-:80
