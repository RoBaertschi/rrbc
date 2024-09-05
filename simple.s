	.file	"simple.c"
	.text
	.globl	main
	.type	main, @function
main:
	movl	$2, %eax
	ret
	.size	main, .-main
	.ident	"GCC: (GNU) 14.2.1 20240805"
	.section	.note.GNU-stack,"",@progbits
