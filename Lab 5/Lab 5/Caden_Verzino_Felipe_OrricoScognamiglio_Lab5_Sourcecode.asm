;***********************************************************
;*
;*	Caden_Verzino_Felipe_OrricoScognamiglio_Lab5_Sourcecode.asm
;*
;*	This program will compute 16-bit sum and subtraction,
;*	24-bit multiplication and compound expression ((D - E) + F)^2
;*
;***********************************************************
;*
;*	 Author: Felipe Orrico Scognamiglio & Caden Verzino
;*	   Date: Feb. 4th, 2021
;*
;***********************************************************

.include "m128def.inc"			; Include definition file

;***********************************************************
;*	Internal Register Definitions and Constants
;***********************************************************
.def	mpr = r16				; Multipurpose register 
.def	rlo = r0				; Low byte of MUL result
.def	rhi = r1				; High byte of MUL result
.def	zero = r2				; Zero register, set to zero in INIT, useful for calculations
.def	A = r3					; A variable
.def	B = r4					; Another variable

.def	oloop = r17				; Outer Loop Counter
.def	iloop = r18				; Inner Loop Counter


;***********************************************************
;*	Start of Code Segment
;***********************************************************
.cseg							; Beginning of code segment

;-----------------------------------------------------------
; Interrupt Vectors
;-----------------------------------------------------------
.org	$0000					; Beginning of IVs
		rjmp 	INIT			; Reset interrupt

.org	$0046					; End of Interrupt Vectors

;-----------------------------------------------------------
; Program Initialization
;-----------------------------------------------------------
INIT:							; The initialization routine
								; Initialize Stack Pointer
		ldi	mpr, low(RAMEND)
		out	SPL, mpr			; Load SPL with low byte of RAMEND
		ldi	mpr, high(RAMEND)
		out	SPH, mpr			; Load SPH with high byte of RAMEND
								; Init the 2 stack pointer registers

		clr		zero			; Set the zero register to zero, maintain
								; these semantics, meaning, don't
								; load anything else into it.

;-----------------------------------------------------------
; Main Program
;-----------------------------------------------------------
MAIN:							; The Main program
		; Setup the ADD16 function direct test

				; Move values 0xFCBA and 0xFFFF in program memory to data memory
				; memory locations where ADD16 will get its inputs from
				; (see "Data Memory Allocation" section below)
				ldi ZL, LOW(ADD16_OP1_PM<<1)	;load low byte of operand from pm to low byte of Z
				ldi ZH, HIGH(ADD16_OP1_PM<<1)	;load high byte of operand from pm to high byte of Z
				ldi YL, LOW(ADD16_OP1)			;load low byte of address from pm to low byte of Y
				ldi YH, HIGH(ADD16_OP1)			;load high byte of address from pm to high byte of Y

				lpm mpr, Z+						;load value stored in address in Z from pm to mpr (low byte)
				st Y+, mpr						;store mpr in dereferenced Y
				lpm mpr, Z						;load value stored in address in Z from pm to mpr (high byte)
				st Y, mpr						;store mpr in dereferenced Y

				ldi ZL, LOW(ADD16_OP2_PM<<1)	;load low byte of operand from pm to low byte of Z
				ldi ZH, HIGH(ADD16_OP2_PM<<1)	;load high byte of operand from pm to high byte of Z
				ldi YL, LOW(ADD16_OP2)			;load low byte of address from pm to low byte of Y
				ldi YH, HIGH(ADD16_OP2)			;load high byte of address from pm to high byte of Y

				lpm mpr, Z+						;load value stored in address in Z from pm to mpr (low byte)
				st Y+, mpr						;store mpr in dereferenced Y
				lpm mpr, Z						;load value stored in address in Z from pm to mpr (high byte)
				st Y, mpr						;store mpr in dereferenced Y

                nop ; Check load ADD16 operands (Set Break point here #1)  
				; Call ADD16 function to test its correctness
				; (calculate FCBA + FFFF)
				Call ADD16

                nop ; Check ADD16 result (Set Break point here #2)
				; Observe result in Memory window

		; Setup the SUB16 function direct test

				; Move values 0xFCB9 and 0xE420 in program memory to data memory
				; memory locations where SUB16 will get its inputs from
				ldi ZL, LOW(SUB16_OP1_PM<<1)	;load low byte of operand from pm to low byte of Z
				ldi ZH, HIGH(SUB16_OP1_PM<<1)	;load high byte of operand from pm to high byte of Z
				ldi YL, LOW(SUB16_OP1)			;load low byte of address from pm to low byte of Y
				ldi YH, HIGH(SUB16_OP1)			;load high byte of address from pm to high byte of Y

				lpm mpr, Z+						;load value stored in address in Z from pm to mpr (low byte)
				st Y+, mpr						;store mpr in dereferenced Y
				lpm mpr, Z						;load value stored in address in Z from pm to mpr (high byte)
				st Y, mpr						;store mpr in dereferenced Y

				ldi ZL, LOW(SUB16_OP2_PM<<1)	;load low byte of operand from pm to low byte of Z
				ldi ZH, HIGH(SUB16_OP2_PM<<1)	;load high byte of operand from pm to high byte of Z
				ldi YL, LOW(SUB16_OP2)			;load low byte of address from pm to low byte of Y
				ldi YH, HIGH(SUB16_OP2)			;load high byte of address from pm to high byte of Y

				lpm mpr, Z+						;load value stored in address in Z from pm to mpr (low byte)
				st Y+, mpr						;store mpr in dereferenced Y
				lpm mpr, Z						;load value stored in address in Z from pm to mpr (high byte)
				st Y, mpr						;store mpr in dereferenced Y

                nop ; Check load SUB16 operands (Set Break point here #3)  
				; Call SUB16 function to test its correctness
				; (calculate FCB9 - E420)
				CALL SUB16
                nop ; Check SUB16 result (Set Break point here #4)
				; Observe result in Memory window

		; Setup the MUL24 function direct test

				; Move values 0xFFFFFF and 0xFFFFFF in program memory to data memory  
				; memory locations where MUL24 will get its inputs from
				ldi ZL, LOW(MUL24_OP1_PM<<1)	;load low byte of operand from pm to low byte of Z
				ldi ZH, HIGH(MUL24_OP1_PM<<1)	;load high byte of operand from pm to high byte of Z
				ldi YL, LOW(MUL24_OP1)			;load low byte of address from pm to low byte of Y
				ldi YH, HIGH(MUL24_OP1)			;load high byte of address from pm to high byte of Y

				lpm mpr, Z+						;load value stored in address in Z from pm to mpr (low byte)
				st Y+, mpr						;store mpr in dereferenced Y
				lpm mpr, Z+						;load value stored in address in Z from pm to mpr ("middle byte)
				st Y+, mpr						;store mpr in (y)
				lpm mpr, Z						;load value stored in address in Z from pm to mpr (high byte)
				st Y, mpr						;store mpr in dereferenced Y

				ldi ZL, LOW(MUL24_OP2_PM<<1)	;load low byte of operand from pm to low byte of Z
				ldi ZH, HIGH(MUL24_OP2_PM<<1)	;load high byte of operand from pm to high byte of Z
				ldi YL, LOW(MUL24_OP2)			;load low byte of address from pm to low byte of Y
				ldi YH, HIGH(MUL24_OP2)			;load high byte of address from pm to high byte of Y

				lpm mpr, Z+						;load value stored in address in Z from pm to mpr (low byte)
				st Y+, mpr						;store mpr in dereferenced Y
				lpm mpr, Z+						;load value stored in address in Z from pm to mpr ("middle" byte)
				st Y+, mpr						;store mpr in (y)
				lpm mpr, Z						;load value stored in address in Z from pm to mpr (high byte)
				st Y, mpr						;store mpr in dereferenced Y

                nop ; Check load MUL24 operands (Set Break point here #5)  
				; Call MUL24 function to test its correctness
				; (calculate FFFFFF * FFFFFF)
				CALL MUL24

                nop ; Check MUL24 result (Set Break point here #6)
				; Observe result in Memory window

				;D
				ldi ZL, LOW(OperandD<<1)		;load low byte of operand from pm to low byte of Z
				ldi ZH, HIGH(OperandD<<1)		;load high byte of operand from pm to high byte of Z
				ldi YL, LOW(COMP_OPD)			;load low byte of address from pm to low byte of Y
				ldi YH, HIGH(COMP_OPD)			;load high byte of address from pm to high byte of Y

				lpm mpr, Z+						;load value stored in address in Z from pm to mpr (low byte)
				st Y+, mpr						;store mpr in dereferenced Y
				lpm mpr, Z						;load value stored in address in Z from pm to mpr (high byte)
				st Y, mpr						;store mpr in dereferenced Y

				;E
				ldi ZL, LOW(OperandE<<1)		;load low byte of operand from pm to low byte of Z
				ldi ZH, HIGH(OperandE<<1)		;load high byte of operand from pm to high byte of Z
				ldi YL, LOW(COMP_OPE)			;load low byte of address from pm to low byte of Y
				ldi YH, HIGH(COMP_OPE)			;load high byte of address from pm to high byte of Y

				lpm mpr, Z+						;load value stored in address in Z from pm to mpr (low byte)
				st Y+, mpr						;store mpr in dereferenced Y
				lpm mpr, Z						;load value stored in address in Z from pm to mpr (high byte)
				st Y, mpr						;store mpr in dereferenced Y

				;F
				ldi ZL, LOW(OperandF<<1)		;load low byte of operand from pm to low byte of Z
				ldi ZH, HIGH(OperandF<<1)		;load high byte of operand from pm to high byte of Z
				ldi YL, LOW(COMP_OPF)			;load low byte of address from pm to low byte of Y
				ldi YH, HIGH(COMP_OPF)			;load high byte of address from pm to high byte of Y

				lpm mpr, Z+						;load value stored in address in Z from pm to mpr (low byte)
				st Y+, mpr						;store mpr in dereferenced Y
				lpm mpr, Z						;load value stored in address in Z from pm to mpr (high byte)
				st Y, mpr						;store mpr in dereferenced Y

                nop ; Check load COMPOUND operands (Set Break point here #7)  
		; Call the COMPOUND function
				Call COMPOUND
                nop ; Check COMPUND result (Set Break point here #8)
				; Observe final result in Memory window

DONE:	rjmp	DONE			; Create an infinite while loop to signify the 
								; end of the program.

;***********************************************************
;*	Functions and Subroutines
;***********************************************************

;-----------------------------------------------------------
; Func: ADD16
; Desc: Adds two 16-bit numbers and generates a 24-bit number
;		where the high byte of the result contains the carry
;		out bit.
;-----------------------------------------------------------
ADD16:
		; Load beginning address of first operand into X
		ldi		XL, low(ADD16_OP1)	; Load low byte of address
		ldi		XH, high(ADD16_OP1)	; Load high byte of address

		; Load beginning address of second operand into Y
		ldi		YL, low(ADD16_OP2)	; Load low byte of address
		ldi		YH, high(ADD16_OP2)	; Load high byte of address

		; Load beginning address of result into Z
		ldi		ZL, low(ADD16_Result)	; Load low byte of address
		ldi		ZH, high(ADD16_Result)	; Load high byte of address

		; Execute the function
		ld r17, X+	;load (X) to r17
		ld R18, Y+	;load (Y) to r18
		add R18,R17	;add r17 and r18
		st Z+, R18	;store r18 to result (low byte)
		ld R17, X	;load (X) to r17
		ld R18, Y	;load (Y) to r18
		adc R18,R17 ;add r17 and r18 with carry
		st Z+, R18	;store r18 to result (low byte)
		brcc EXIT	;check if carry flag is not set
		st Z,XH		;store (XH) to (Z)
		EXIT:
		
		ret						; End a function with RET

;-----------------------------------------------------------
; Func: SUB16
; Desc: Subtracts two 16-bit numbers and generates a 16-bit
;		result.
;-----------------------------------------------------------
SUB16:
		; Execute the function here
		; Load beginning address of first operand into X
		ldi		XL, low(SUB16_OP1)	; Load low byte of address
		ldi		XH, high(SUB16_OP1)	; Load high byte of address

		; Load beginning address of second operand into Y
		ldi		YL, low(SUB16_OP2)	; Load low byte of address
		ldi		YH, high(SUB16_OP2)	; Load high byte of address

		; Load beginning address of result into Z
		ldi		ZL, low(SUB16_Result)	; Load low byte of address
		ldi		ZH, high(SUB16_Result)	; Load high byte of address

		ld r17, X+	; Load (X) to R17
		ld R18, Y+	; Load (Y) to R18
		sub R17,R18 ; Rsubtract
		st Z+, R17	; store result to (Z)
		ld R17, X	; Load (X) to R17
		ld R18, Y	; Load (Y) to R18
		sbc R17,R18 ; subtract with carry
		st Z+, R17	; Store result to Z

		ret						; End a function with RET

;-----------------------------------------------------------
; Func: MUL24
; Desc: Multiplies two 24-bit numbers and generates a 48-bit 
;		result.
;-----------------------------------------------------------
MUL24:
		; Execute the function here
		push 	A				; Save A register
		push	B				; Save B register
		push	rhi				; Save rhi register
		push	rlo				; Save rlo register
		push	zero			; Save zero register
		push	XH				; Save X-ptr
		push	XL
		push	YH				; Save Y-ptr
		push	YL				
		push	ZH				; Save Z-ptr
		push	ZL
		push	oloop			; Save counters
		push	iloop				

		clr		zero			; Maintain zero semantics

		; Set Y to beginning address of B
		ldi		YL, low(MUL24_OP1)	; Load low byte
		ldi		YH, high(MUL24_OP1)	; Load high byte

		; Set Z to begginning address of resulting Product
		ldi		ZL, low(MUL24_Result)	; Load low byte
		ldi		ZH, high(MUL24_Result); Load high byte

		; Begin outer for loop
		ldi		oloop, 3		; Load counter
MUL24_OLOOP:
		; Set X to beginning address of A
		ldi		XL, low(MUL24_OP2)	; Load low byte
		ldi		XH, high(MUL24_OP2)	; Load high byte

		; Begin inner for loop
		ldi		iloop, 3		; Load counter
MUL24_ILOOP:
		ld		A, X+			; Get byte of A operand
		ld		B, Y			; Get byte of B operand
		mul		A,B				; Multiply A and B
		ld		A, Z+			; Get a result byte from memory
		ld		B, Z+			; Get the next result byte from memory
		add		rlo, A			; rlo <= rlo + A
		adc		rhi, B			; rhi <= rhi + B + carry
		ld		A, Z+			; Get a third byte from the result
		ld		R19, Z			; Get the 4th byte
		adc		A, zero			; Add carry to A
		adc		R19, zero		; Add carry to R19
		st		Z, R19			; Store fourth byte to memory
		st		-Z, A			; Store third byte to memory
		st		-Z, rhi			; Store second byte to memory
		st		-Z, rlo			; Store first byte to memory
		adiw	ZH:ZL, 1		; Z <= Z + 1			
		dec		iloop			; Decrement counter
		brne	MUL24_ILOOP		; Loop if iLoop != 0
		; End inner for loop

		sbiw	ZH:ZL, 2		; Z <= Z - 2
		adiw	YH:YL, 1		; Y <= Y + 1
		dec		oloop			; Decrement counter
		brne	MUL24_OLOOP		; Loop if oLoop != 0
		; End outer for loop
		 		
		pop		iloop			; Restore all registers in reverves order
		pop		oloop
		pop		ZL				
		pop		ZH
		pop		YL
		pop		YH
		pop		XL
		pop		XH
		pop		zero
		pop		rlo
		pop		rhi
		pop		B
		pop		A

		ret						; End a function with RET

;-----------------------------------------------------------
; Func: CLEAR_RESULTS
; Desc: Clears the results of ADD, SUB, and MULT
;-----------------------------------------------------------
CLEAR_RESULTS:
		;clearing bytes
		clr zero ;store R2 to results
		ldi XL, LOW(MUL24_Result) ;load MUL24 results memory location
		ldi XH, HIGH(MUL24_Result)
		st X+, zero	;store zero to MUL24 results memory location's value
		st X+, zero
		st X+, zero
		st X+, zero
		st X+, zero
		st X+, zero
		ldi XL, LOW(ADD16_Result) ;load ADD16 results memory location
		ldi XH, HIGH(ADD16_Result)
		st X+, zero ;store zero to ADD16 results memory location's value
		st X+, zero
		st X+, zero
		ldi XL, LOW(SUB16_Result) ;load SUB16 results memory location
		ldi XH, HIGH(SUB16_Result)
		st X+, zero	;store zero to SUB16 results memory location's value
		st X+, zero
		st X+, zero
		ldi XL, LOW(MUL24_OP1) ;load MUL24 op1 memory location
		ldi XH, HIGH(MUL24_OP1)
		st X+, zero	;store zero to MUL24 op1 memory location's value
		st X+, zero
		st X+, zero
		ldi XL, LOW(MUL24_OP2) ;load MUL24 op2 memory location
		ldi XH, HIGH(MUL24_OP2)
		st X+, zero ;store zero to MUL24 op2 memory location's value
		st X+, zero
		st X+, zero
	ret
;-----------------------------------------------------------
; Func: COMPOUND
; Desc: Computes the compound expression ((D - E) + F)^2
;		by making use of SUB16, ADD16, and MUL24.
;
;		D, E, and F are declared in program memory, and must
;		be moved into data memory for use as input operands.
;
;		All result bytes should be cleared before beginning.
;-----------------------------------------------------------
COMPOUND:

		RCall CLEAR_RESULTS
		; Setup SUB16 with operands D and E
		; Perform subtraction to calculate D - E

		;op1
		ldi ZL, LOW(OperandD<<1) ;load memory to the D operand to Z
		ldi ZH, HIGH(OperandD<<1)
		ldi XL, LOW(SUB16_OP1) ;load memory to SUB16_OP1 to X
		ldi XH, HIGH(SUB16_OP1)

		lpm mpr, Z+ ;load low byte of operandD to mpr
		st X+, mpr ;store low byte of operandD to XL
		lpm mpr, Z ;load high byte of operandD to mpr
		st X, mpr	;store high byte of operandD to XH

		;op2
		ldi ZL, LOW(OperandE<<1) ;load memory to the E operand to Z
		ldi ZH, HIGH(OperandE<<1)
		ldi XL, LOW(SUB16_OP2)	;load memory to SUB16_OP2 to X
		ldi XH, HIGH(SUB16_OP2)

		lpm mpr, Z+ ;load low byte of operandE to mpr
		st X+, mpr ;store low byte of operandE to XL
		lpm mpr, Z ;load high byte of operandE to mpr
		st X, mpr ;store high byte of operandE to XH

		Call SUB16 ;call sub16 function

		; Setup the ADD16 function with SUB16 result and operand F
		; Perform addition next to calculate (D - E) + F

		;Operator 1
		ldi ZL, LOW(SUB16_Result) ;load memory of SUB16 results to Z
		ldi ZH, HIGH(SUB16_Result)
		ldi XL, LOW(ADD16_OP1)	;load memory of ADD16_OP1 to X
		ldi XH, HIGH(ADD16_OP1)

		ld mpr, Z+	;load low byte of SUB16 results to mpr
		st X+, mpr	;store low byte of SUB16 results to ADD16_OP1
		ld mpr, Z	;load high byte of SUB16 results to mpr
		st X, mpr	;store high byte of SUB16 results to ADD16_OP1

		;Operator 2
		ldi ZL, LOW(OperandF<<1)	;load memory to the F operand to Z
		ldi ZH, HIGH(OperandF<<1)
		ldi XL, LOW(ADD16_OP2)	;load memory of ADD16_OP2 to X
		ldi XH, HIGH(ADD16_OP2)

		lpm mpr, Z+ ;load low byte of OperandF to mpr
		st X+, mpr	;store low byte of OperandF to ADD16_OP2
		lpm mpr, Z	;load high byte of OperandF results to mpr
		st X, mpr	;store high byte of OperandF results to ADD16_OP2

		Call ADD16 ;call add16 function

		; Setup the MUL24 function with ADD16 result as both operands
		; Perform multiplication to calculate ((D - E) + F)^2

		;OPERATOR 1
		ldi ZL, LOW(ADD16_Result) ;load memory of ADD16 results to Z
		ldi ZH, HIGH(ADD16_Result)
		ldi XL, LOW(MUL24_OP1)	;load memory of MUL24_OP1 to X
		ldi XH, HIGH(MUL24_OP1)

		ld mpr, Z+	;load low byte of ADD16 results to mpr
		st X+, mpr	;store low byte of ADD16 results to MUL24_OP1
		ld mpr, Z	;load high byte of ADD16 results to mpr
		st X, mpr	;store high byte of ADD16 results to MUL24_OP1

		;OPERATOR 2
		ldi ZL, LOW(ADD16_Result)	;load memory of ADD16 results to Z
		ldi ZH, HIGH(ADD16_Result)
		ldi XL, LOW(MUL24_OP2)	;load memory of MUL24_OP2 to X
		ldi XH, HIGH(MUL24_OP2)

		ld mpr, Z+	;load low byte of ADD16 results to mpr
		st X+, mpr	;store low byte of ADD16 results to MUL24_OP2
		ld mpr, Z	;load high byte of ADD16 results to mpr
		st X, mpr	;store high byte of ADD16 results to MUL24_OP2

		Call MUL24 ;Call mul24 function and get squared

		;COMP_Result
		ldi ZL, LOW(MUL24_Result) ;load memory of MUL24 results to Z
		ldi ZH, HIGH(MUL24_Result)
		ldi XL, LOW(COMP_Result)	;load memory of COMP_Result to X
		ldi XH, HIGH(COMP_Result)
		ldi R20, 6 ;initialize R20 for 6 loops
		CMP_RES:
			cpi R20, 0
			BREQ CMP_DONE ;stop loop if R20 is 0
			ld mpr, Z+	;load low/high byte of MUL24 results to mpr
			st X+, mpr	;store low/high byte of MUL24 results to COMP_Result
			dec R20		;decrement r20
			rjmp CMP_RES;jump to beginning of the loop
		CMP_DONE:

		ret						; End a function with RET

;-----------------------------------------------------------
; Func: MUL16
; Desc: An example function that multiplies two 16-bit numbers
;			A - Operand A is gathered from address $0101:$0100
;			B - Operand B is gathered from address $0103:$0102
;			Res - Result is stored in address 
;					$0107:$0106:$0105:$0104
;		You will need to make sure that Res is cleared before
;		calling this function.
;-----------------------------------------------------------
MUL16:
		push 	A				; Save A register
		push	B				; Save B register
		push	rhi				; Save rhi register
		push	rlo				; Save rlo register
		push	zero			; Save zero register
		push	XH				; Save X-ptr
		push	XL
		push	YH				; Save Y-ptr
		push	YL				
		push	ZH				; Save Z-ptr
		push	ZL
		push	oloop			; Save counters
		push	iloop				

		clr		zero			; Maintain zero semantics

		; Set Y to beginning address of B
		ldi		YL, low(addrB)	; Load low byte
		ldi		YH, high(addrB)	; Load high byte

		; Set Z to beginning address of resulting Product
		ldi		ZL, low(LAddrP)	; Load low byte
		ldi		ZH, high(LAddrP); Load high byte

		; Begin outer for loop
		ldi		oloop, 2		; Load counter
MUL16_OLOOP:
		; Set X to beginning address of A
		ldi		XL, low(addrA)	; Load low byte
		ldi		XH, high(addrA)	; Load high byte

		; Begin inner for loop
		ldi		iloop, 2		; Load counter
MUL16_ILOOP:
		ld		A, X+			; Get byte of A operand
		ld		B, Y			; Get byte of B operand
		mul		A,B				; Multiply A and B
		ld		A, Z+			; Get a result byte from memory
		ld		B, Z+			; Get the next result byte from memory
		add		rlo, A			; rlo <= rlo + A
		adc		rhi, B			; rhi <= rhi + B + carry
		ld		A, Z			; Get a third byte from the result
		adc		A, zero			; Add carry to A
		st		Z, A			; Store third byte to memory
		st		-Z, rhi			; Store second byte to memory
		st		-Z, rlo			; Store first byte to memory
		adiw	ZH:ZL, 1		; Z <= Z + 1			
		dec		iloop			; Decrement counter
		brne	MUL16_ILOOP		; Loop if iLoop != 0
		; End inner for loop

		sbiw	ZH:ZL, 1		; Z <= Z - 1
		adiw	YH:YL, 1		; Y <= Y + 1
		dec		oloop			; Decrement counter
		brne	MUL16_OLOOP		; Loop if oLoop != 0
		; End outer for loop
		 		
		pop		iloop			; Restore all registers in reverse order
		pop		oloop
		pop		ZL				
		pop		ZH
		pop		YL
		pop		YH
		pop		XL
		pop		XH
		pop		zero
		pop		rlo
		pop		rhi
		pop		B
		pop		A
		ret						; End a function with RET


;***********************************************************
;*	Stored Program Data
;***********************************************************

; Enter any stored data you might need here

; ADD16 operands
ADD16_OP1_PM:
	.DW	0xFCBA				; test value for operand 1 of add16
ADD16_OP2_PM:
	.DW	0xFFFF				; test value for operand 2 of add16
; SUB16 operands 0xFCB9 and 0xE420
SUB16_OP1_PM:
	.DW	0xFCB9				; test value for operand 1 of sub16
SUB16_OP2_PM:
	.DW	0xE420				; test value for operand 2 of sub16
; MUL24 operands
MUL24_OP1_PM:
	.DW	0xFFFFFF			; test value for operand 1 of MUL24
MUL24_OP2_PM:
	.DW	0xFFFFFF		    ; test value for operand 2 of MUL24
MUL24_OPextra_PM:
	.DW	0xFFFF				; test value for operand extra of MUL24
; Compound operands
OperandD:
	.DW	0xFCBA				; test value for operand D
OperandE:
	.DW	0x2019				; test value for operand E
OperandF:
	.DW	0x21BB				; test value for operand F

;***********************************************************
;*	Data Memory Allocation
;***********************************************************

.dseg
.org	$0100				; data memory allocation for MUL16 example
addrA:	.byte 2
addrB:	.byte 2
LAddrP:	.byte 4

; Below is an example of data memory allocation for ADD16.
; Consider using something similar for SUB16 and MUL24.

.org	$0110				; data memory allocation for operands
ADD16_OP1:
		.byte 2				; allocate two bytes for first operand of ADD16
ADD16_OP2:
		.byte 2				; allocate two bytes for second operand of ADD16

.org	$0120				; data memory allocation for results
ADD16_Result:
		.byte 3				; allocate three bytes for ADD16 result

.org	$0130				; data memory allocation for operands
SUB16_OP1:
		.byte 2				; allocate two bytes for first operand of SUB16
SUB16_OP2:
		.byte 2				; allocate two bytes for second operand of SUB16

.org	$0140				; data memory allocation for results
SUB16_Result:
		.byte 2				; allocate three bytes for SUB16 result

.org $0149					; data memory allocation for operands
MUL24_OP1:
		.byte 3				; allocate two bytes for first operand of MUL24
MUL24_OP2:
		.byte 3				; allocate two bytes for second operand of MUL24

.org	$0160				; data memory allocation for results
MUL24_Result:
		.byte 6				; allocate three bytes for MUL24 result

.org $0170					; data memory allocation for operands
COMP_OPD:
		.byte 2				; allocate two bytes for first operand of COMP
COMP_OPE:
		.byte 2				; allocate two bytes for second operand of COMP

COMP_OPF:
		.byte 2				; allocate two bytes for second operand of COMP

.org	$0180				; data memory allocation for results
COMP_Result:
		.byte 6				; allocate three bytes for COMP4 result

;***********************************************************
;*	Additional Program Includes
;***********************************************************
; There are no additional file includes for this program