EXTERN	_start
EXTERN	AsciiDigit	; al = digit
EXTERN	Crlf		; no param
EXTERN	Exit		; ebx = ret code (syscall)
EXTERN	Strlen		; edi = string, RET: eax = the length
EXTERN	Write		; ecx = buffer, edx = count, RET: eax = size written (syscall)
EXTERN	WriteChar	; al = the char
EXTERN	WriteHex	; eax = the number
EXTERN	WriteInt	; eax = the number
EXTERN	WriteString	; edx = the string
