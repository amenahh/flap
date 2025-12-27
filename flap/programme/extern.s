.data
.text
	.globl main
.p2align 3, 144
main:
	/* Program entry point. */
	subq $8, %rsp
	call .I_129913994
	movq $0, %rdi
	call exit
.p2align 3, 144
.I_129913994:
	/* Initializer for . */
	movq $42, %rdi
	call observe_int
	movq $0, %rdi
	call exit
