# trap.S
# In the future our trap vector will go here.

.global uart_print_hex
uart_print_hex:
    addi    sp, sp, -16
    sd      ra, 8(sp)

    li      t1, 60              # Start at highest nibble (bit 60)
1:
    srl     t2, a0, t1          # Shift right by t1 to get current nibble
    andi    t2, t2, 0xf         # Mask lower 4 bits

    li      t3, 10
    blt     t2, t3, 2f
    addi    t2, t2, 87          # 87 = 'a' - 10
    j       3f
2:
    addi    t2, t2, 48          # 48 = '0'
3:
    li      t4, 0x10000000
    sb      t2, 0(t4)           # Output character

    addi    t1, t1, -4
    bgez    t1, 1b              # Loop until t1 < 0

    ld      ra, 8(sp)
    addi    sp, sp, 16
    ret

.section .text
.global uart_putchar
uart_putchar:
    li t0, 0x10000000     # UART0 MMIO address
    sb a0, 0(t0)          # Store byte from a0 to UART0
    ret

.global asm_trap_vector
# This will be our trap vector when we start
# handling interrupts.
asm_trap_vector:
    # Print "TRAP!\n"
    li      a0, 'T'
    call    uart_putchar
    li      a0, 'R'
    call    uart_putchar
    li      a0, 'A'
    call    uart_putchar
    li      a0, 'P'
    call    uart_putchar
    li      a0, '!'
    call    uart_putchar
    li      a0, 10              # '\n'
    call    uart_putchar

    # Print scause
    li      a0, 'C'
    call    uart_putchar
    li      a0, ':'
    call    uart_putchar
    li      a0, ' '
    call    uart_putchar
    csrr    a0, scause
    call    uart_print_hex
    li      a0, 10
    call    uart_putchar

    # Print sepc
    li      a0, 'P'
    call    uart_putchar
    li      a0, ':'
    call    uart_putchar
    li      a0, ' '
    call    uart_putchar
    csrr    a0, sepc
    call    uart_print_hex
    li      a0, 10
    call    uart_putchar

    # Print stval (e.g., bad virtual address)
    li      a0, 'V'
    call    uart_putchar
    li      a0, ':'
    call    uart_putchar
    li      a0, ' '
    call    uart_putchar
    csrr    a0, stval
    call    uart_print_hex
    li      a0, 10
    call    uart_putchar

trap_loop:
    wfi
    j trap_loop

