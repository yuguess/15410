/** @file read_ebp.S
 *  @brief The function read register ebp value during runtime
 *  @author Dalong Cheng(dalongc)
 *  @bug No known bugs
 */
.global read_ebp

read_ebp:
	pushl	%ebp		
	movl	%esp,%ebp	
	pushl	%edi
	pushl	%ebx
	pushl	%esi
	
        movl	%ebp,%eax	/* get the argument */

	/* restore the callee save registers */
	popl	%esi
	popl	%ebx
	popl	%edi
	movl	%ebp,%esp
	popl	%ebp
	ret

