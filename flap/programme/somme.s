.data
S:
	/* S */
	.quad 0
.text
	.globl main
.p2align 3, 144
main:
	/* Program entry point. */
	subq $8, %rsp
	call .I_730585739
	movq $0, %rdi
	call exit
.p2align 3, 144
.I_730585739:
	/* Initializer for S. */
	movq $1, %rdi
	movq $2, %rsi
	movq $3, %rdx
	movq $4, %rcx
	movq $5, %r8
	movq $6, %r9
	pushq $1000
	pushq $800
	call add_eight_int
	movq %rax, S(%rip)
	movq S(%rip), %rdi
	call observe_int
	movq $0, %rdi
	call exit
