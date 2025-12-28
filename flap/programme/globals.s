.data
z:
	/* z */
	.quad 0
y:
	/* y */
	.quad 0
x:
	/* x */
	.quad 0
k:
	/* k */
	.quad 0
.text
	.globl main
.p2align 3, 144
main:
	/* Program entry point. */
	subq $8, %rsp
	call .I_570419179
	movq $0, %rdi
	call exit
.p2align 3, 144
.I_570419179:
	/* Initializer for x, y, z, k. */
	pushq %rbp
	movq %rsp, %rbp
	subq $0, %rsp
	movq $6, x(%rip)
	movq $7, y(%rip)
	movq y(%rip), %r15
	imulq x(%rip), %r15
	movq %r15, z(%rip)
	movq z(%rip), %r15
	subq x(%rip), %r15
	movq %r15, k(%rip)
	ret
