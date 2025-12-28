.data
.text
	.globl main
.p2align 3, 144
main:
	/* Program entry point. */
	subq $8, %rsp
	movq $0, %rdi
	call exit
