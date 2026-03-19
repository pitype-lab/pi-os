.section .text
.global m_trap_vector
.align 4
m_trap_vector:
  # Fast path: ecall from S-mode (cause 9) to set mscratch
  csrr  t0, mcause
  li    t1, 9
  bne   t0, t1, 1f
  # ecall from S-mode: mscratch = a0, mepc += 4
  csrw  mscratch, a0
  csrr  t0, mepc
  addi  t0, t0, 4
  csrw  mepc, t0
  mret
1:

  # Enable FPU
  csrr  t0, mstatus
	li    t1, (1 << 13)     # FS = 01 (Initial state)
	or    t0, t0, t1
	csrw  mstatus, t0

	csrr	a0, mepc
	csrr	a1, mtval
	csrr	a2, mcause
	csrr	a3, mhartid
	csrr	a4, mstatus
	call	m_trap

	# m_trap returns new epc in a0 — write it back
	csrw	mepc, a0

	mret

.global make_syscall
make_syscall:
	ecall
	ret


