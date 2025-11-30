.option norvc

.section .text.init

.global _start
_start:
.option push
.option norelax
	la		gp, _global_pointer
.option pop
  # Ensure SATP = 0 initially
	csrw	satp, zero

  # Zero the .bss section
	la 		a0, _bss_start
	la		a1, _bss_end
	bgeu	a0, a1, 2f
1:
	sd		zero, (a0)
	addi	a0, a0, 8
	bltu	a0, a1, 1b
2:
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

	la		ra, 2f
	mret
2:
  # Now still in machine mode; a0 = SATP value
  csrw    satp, a0
  sfence.vma

  # Set PMP to allow full access
	li t0, -1                # Max addressable memory
	csrw pmpaddr0, t0        # Set PMP region 0 to cover all memory
	li t0, 0xF               # R/W/X, TOR mode
	csrw pmpcfg0, t0         # Enable full access

  # Machine trap vector
	la		t2, m_trap_vector
	csrw	mtvec, t2

	# Setting `mstatus` (supervisor status) register:
	li		t0, (1 << 13) | (0b01 << 11) | (1 << 7) | (1 << 5)
	csrw	mstatus, t0

  # Entry point for S-mode
	la		t1, 3f
	csrw	mepc, t1

  # Enable machine interrupts (SOFT | TIMER | EXTERNAL)
	li		t2, (1 << 1) | (1 << 5) | (1 << 9)
	csrw	mie, t2

	mret
3:
  jal     ra, main
4:
	wfi
	j		4b

