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

	# Do not allow interrupts while running in M-mode init
	csrw	mie, zero

	# Enable FPU: set FS field in mstatus to 0b01 (Initial)
	li		t0, (0b11 << 11) | (1 << 13)
	csrw	mstatus, t0

  # Call main directly in M-mode
	jal		ra, main

  # Should not return, but if it does, spin
1:
	wfi
	j		1b

# ---------------------------------------------------------------
# enter_supervisor_mode(satp_value)
#   a0 = SATP register value (mode | ASID | PPN)
#
# Called from Idris via FFI. Sets up SATP, PMP, trap vector,
# then mret into S-mode. Returns to the caller's ra but now
# running in S-mode with the MMU enabled.
# ---------------------------------------------------------------
.global enter_supervisor_mode
.align 4
enter_supervisor_mode:
  # Set SATP and flush TLB
	csrw	satp, a0
	sfence.vma

  # Set PMP to allow full access
	li		t0, -1
	csrw	pmpaddr0, t0
	li		t0, 0xF               # R/W/X, TOR mode
	csrw	pmpcfg0, t0

  # Machine trap vector
	la		t2, m_trap_vector
	csrw	mtvec, t2

  # Set mstatus: MPP=01 (S-mode), FS=01 (FPU), MPIE=1, SPIE=1
	li		t0, (1 << 13) | (0b01 << 11) | (1 << 7) | (1 << 5)
	csrw	mstatus, t0

  # Enable M-mode external interrupts only (MEIE)
	li		t2, (1 << 11)
	csrw	mie, t2

  # Set mepc to our return address so mret jumps back to caller in S-mode
	csrw	mepc, ra

  # mret: drops to S-mode, jumps to mepc (= ra)
	mret
