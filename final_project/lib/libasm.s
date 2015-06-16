	EXTERN	main

	SECTION	.text
	GLOBAL	_start
_start:
	call	main
	mov	ebx, eax
	call	Exit
	ret

	GLOBAL	AsciiDigit
AsciiDigit:	; al = digit to convert
	SECTION	.data
.xtable	db	"0123456789abcdef"
	SECTION	.text
	push	ebx
	mov	ebx, .xtable
	xlat
	pop	ebx
	ret

	GLOBAL	Crlf
Crlf:
	SECTION	.data
.lf	db	0x0a
.lflen	equ	$-.lf
	SECTION	.text
	pusha
	mov	ecx, .lf
	mov	edx, .lflen
	call	Write
	popa
	ret

	GLOBAL	Exit
Exit:					; ebx = error code
	mov	eax, 0x01
	int	0x80
	ret

	GLOBAL	Strlen
Strlen:					; edi = string
	push	edi
	mov	eax, 0
.L1:
	cmp	byte [edi], 0		; end of string?
	je	.L2
	inc	edi
	inc	eax
	jmp	.L1
.L2:
	pop	edi
	ret

	GLOBAL	Write
Write:					; ecx = buffer, edx = count
	push	ebx
	mov	eax, 0x04		; sys_write = 4
	mov	ebx, 1			; fd = 1 (stdout)
	int	0x80
	pop	ebx
	ret

	GLOBAL	WriteChar
WriteChar:				; al = the char
	SECTION	.data
.bb	db	0
	SECTION	.text
	pushad
	mov	byte [.bb], al
	mov	ecx, .bb
	mov	edx, 1
	call	Write
	popad
	ret

	GLOBAL	WriteHex
WriteHex:				; eax = the number
	SECTION	.data
.buf	TIMES 16 db 0			; buffer
.bufsz	equ	$-.buf			; bufsize
	SECTION	.text
	pushad
	mov	edi, .buf		; point to the string
	add	edi, .bufsz
	mov	ecx, 0			; buflen
.L1:
	mov	ebx, eax		; preserv eax into ebx
	and	al, 0x0f
	call	AsciiDigit
	dec	edi
	mov	byte [edi], al
	inc	ecx
	mov	eax, ebx		; restore eax
	shr	eax, 4
	or	eax, eax		; eax == zero ?
	jnz	.L1
	;
	mov	edx, ecx
	mov	ecx, edi
	call	Write
	popad
	ret

	GLOBAL	WriteInt
WriteInt:				; eax = the number
	SECTION	.data
.isneg	db	0			; negative = 0 (false)
.buf	TIMES 16 db 0			; buffer
.bufsz	equ	$-.buf			; bufsize
	SECTION	.text
	pushad
	mov	byte [.isneg], 0	; negative = 0 (false)
	or	eax, eax		; eax is positive?
	jns	.L1
	neg	eax
	mov	byte [.isneg], 1	; negative = 1 (true)
.L1:
	mov	edi, .buf		; point to the string
	add	edi, .bufsz
	mov	ecx, 0			; buflen
	mov	ebx, 10			; divided by 10
.L2:
	mov	edx, 0			; edx:eax = the number
	div	ebx			; eax = Q, edx = R
	or	dl, 0x30		; convert value to ASCII
	dec	edi
	mov	byte [edi], dl
	inc	ecx
	or	eax, eax		; eax == zero ?
	jnz	.L2
	; add the sign symbol
	cmp	byte [.isneg], 0
	je	.L3
	dec	edi
	mov	byte [edi], '-'
	inc	ecx
.L3:
	mov	edx, ecx
	mov	ecx, edi
	call	Write
	popad
	ret

	GLOBAL	WriteString
WriteString:
	SECTION	.text
	pushad
	mov	edi, edx
	call	Strlen
	mov	ecx, edx
	mov	edx, eax
	call	Write
	popad
	ret

