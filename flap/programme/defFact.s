.data
x:
	/* x */
	.quad 0
.text
	.globl main
.p2align 3, 144
main:
	/* Program entry point. */
	subq $8, %rsp
	call .I_154575915
	movq $0, %rdi
	call exit
.p2align 3, 144
fact:
	/* Retrolix function fact. */
	pushq %rbp
	movq %rsp, %rbp
	subq $0, %rsp
	movq $1, %rax
f2:
	cmpq $0, %rdi
	je f6
	jmp f3
f3:
	movq %rax, %r15
	imulq %rdi, %r15
	movq %r15, %rax
	movq %rdi, %r15
	subq $1, %r15
	movq %r15, %rdi
	jmp f2
f6:
	addq $0, %rsp
	popq %rbp
	ret
.p2align 3, 144
rec_fact:
	/* Retrolix function rec_fact. */
	pushq %rbp
	movq %rsp, %rbp
	subq $8, %rsp
	movq $1, %rax
	cmpq $0, %rdi
	je l7
	jmp l3
l3:
	movq %rdi, -8(%rbp)
	movq %rdi, %r15
	subq $1, %r15
	movq %r15, %rdi
	call rec_fact
	movq %rax, %r15
	imulq -8(%rbp), %r15
	movq %r15, %rax
l7:
	addq $8, %rsp
	popq %rbp
	ret
.p2align 3, 144
.I_154575915:
	/* Initializer for x. */
	pushq %rbp
	movq %rsp, %rbp
	subq $0, %rsp
	movq $5, %rdi
	call fact
	movq %rax, x(%rip)
	movq x(%rip), %rdi
	call observe_int
