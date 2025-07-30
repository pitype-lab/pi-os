# trap.S
# Trap handler and global context
# Steve Operating System
# Stephen Marz
# 24 February 2019

.global m_trap_vector
m_trap_vector:
	csrr  t0, mstatus       # Read mstatus
	li    t1, (1 << 13)     # FS = 01 (Initial state)
	or    t0, t0, t1
	csrw  mstatus, t0       # Write back to enable FPU

	# Get ready to go into Rust (trap.rs)
	# We don't want to write into the user's stack or whomever
	# messed with us here.
	csrr	a0, mepc
	csrr	a1, mtval
	csrr	a2, mcause
	csrr	a3, mhartid
	csrr	a4, mstatus
	call	m_trap

	mret

