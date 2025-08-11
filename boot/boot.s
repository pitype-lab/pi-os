# boot.S
# bootloader for SoS
# Stephen Marz
# 8 February 2019

# Disable generation of compressed instructions.
.option norvc

# Define a .text.init section. The .text.init is put at the
# starting address so that the entry _start is put at the RISC-V
# address 0x8000_0000.
.section .text.init

# Execution starts here.
.global _start
_start:

	# Disable linker instruction relaxation for the `la` instruction below.
	# This disallows the assembler from assuming that `gp` is already initialized.
	# This causes the value stored in `gp` to be calculated from `pc`.
	# The job of the global pointer is to give the linker the ability to address
	# memory relative to GP instead of as an absolute address.
.option push
.option norelax
	la		gp, _global_pointer
.option pop
	# SATP should be zero, but let's make sure. Each HART has its own
	# SATP register.
	csrw	satp, zero
	# Any hardware threads (hart) that are not bootstrapping
	# need to wait for an IPI
	csrr	t0, mhartid
	bnez	t0, 3f

	# Set all bytes in the BSS section to zero.
	la 		a0, _bss_start
	la		a1, _bss_end
	bgeu	a0, a1, 2f
1:
	sd		zero, (a0)
	addi	a0, a0, 8
	bltu	a0, a1, 1b
2:
	# The stack grows from bottom to top, so we put the stack pointer
	# to the very end of the stack range.
	la		sp, _stack_end
	# Setting `mstatus` register:
	# 0b01 << 11: Machine's previous protection mode is 2 (MPP=2).
	li		t0, 0b11 << 11
	csrw	mstatus, t0
	# Do not allow interrupts while running kinit
	csrw	mie, zero
	# Enable FPU: set FS field in mstatus to 0b01 (Initial) or 0b11 (Dirty)
	csrr	t0, mstatus
	li	t1, (1 << 13)        # FS = 0b01 (Initial)
	or	t0, t0, t1
	csrw mstatus, t0
	# Machine's exception program counter (MEPC) is set to `kinit`.
	la		t1, kinit
	csrw	mepc, t1

	# Set the return address to get us into supervisor mode
	la		ra, 2f
	# We use mret here so that the mstatus register is properly updated.
	mret
2:
	# We set the return address (ra above) to this label. When kinit() is finished
	# in Rust, it will return here.

	# Setting `mstatus` (supervisor status) register:
	# 1 << 8    : Supervisor's previous protection mode is 1 (SPP=1 [Supervisor]).
	# 1 << 5    : Supervisor's previous interrupt-enable bit is 1 (SPIE=1 [Enabled]).
	# 1 << 1    : Supervisor's interrupt-enable bit will be set to 1 after sret.
	# We set the "previous" bits because the sret will write the current bits
	# with the previous bits.
	li		t0, (1 << 13) | (0b01 << 11) | (1 << 7) | (1 << 5)
	csrw	mstatus, t0
	la		t2, m_trap_vector
	csrw	mtvec, t2
	la		t1, main
	csrw	mepc, t1

	# kinit() is required to return back the SATP value (including MODE) via a0
	csrw	satp, a0
	# Force the CPU to take our SATP register.
	# To be efficient, if the address space identifier (ASID) portion of SATP is already
	# in cache, it will just grab whatever's in cache. However, that means if we've updated
	# it in memory, it will be the old table. So, sfence.vma will ensure that the MMU always
	# grabs a fresh copy of the SATP register and associated tables.
	sfence.vma
	# sret will put us in supervisor mode and re-enable interrupts

	li t0, -1                # Max addressable memory
	csrw pmpaddr0, t0        # Set PMP region 0 to cover all memory
	li t0, 0xF               # R/W/X, TOR mode
	csrw pmpcfg0, t0         # Enable full access

	li		t2, (1 << 1) | (1 << 5) | (1 << 9)
	csrw	mie, t2

	mret
3:

4:
	# wfi = wait for interrupt. This is a hint to the harts to shut everything needed
	# down. However, the RISC-V specification allows for wfi to do nothing. Anyway,
	# with QEMU, this will save some CPU!
	wfi
	j		4b
