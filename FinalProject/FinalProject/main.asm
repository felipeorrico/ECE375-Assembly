;***********************************************************
;*	 Author: Felipe Orrico Scognamiglio
;*   Date: March 15, 2021
;***********************************************************
.include "m128def.inc"			; Include definition file
;***********************************************************
;*	Internal Register Definitions and Constants
;*	(feel free to edit these or add others)
;***********************************************************
.def	rlo = r0				; Low byte of MUL result
.def	rhi = r1				; High byte of MUL result
.def	zero = r23				; Zero register, set to zero in INIT, useful for calculations
.def	A = r3					; A variable
.def	B = r4					; Another variable
.def	mpr = r16				; Multipurpose register 
.def	oloop = r17				; Outer Loop Counter
.def	iloop = r18				; Inner Loop Counter
.def	dataptr = r19			; data ptr

;***********************************************************
;*	Data segment variables
;*	(feel free to edit these or add others)
;***********************************************************
.dseg
.org	$0100						; data memory allocation for operands
COMPARE_RADIUS:		.byte 2			; allocate 2 bytes for a variable to compare radius to min radius
COMPARE_GM:			.byte 4			; allocate 4 bytes for a variable to compare GM to min GM
COMPARE_PERIOD:		.byte 3			; allocate 3 bytes for a variable to compare period to min period
COMPARE_V:			.byte 2			; allocate 2 bytes for a variable to compare velocity to min velocity
.org	$0200
PlanetGM:		.byte 4
.org	$0210
Radius:			.byte 2

;
;	Allocating Memory used for operands and partial values
;	Please no not modify any of these values below
;

.org	$0220
DIV32_OP1:
		.byte 4	
DIV32_OP2:
		.byte 4
DIV32_Result:
		.byte 3
DIV32_Rem:
		.byte 4
.org	$0250
SQRT_32_CNT:				
		.byte 3
.org	$0260
SQRT_32_VAL:				
		.byte 4
.org	$0270
MUL24_OP1:
		.byte 3	
MUL24_OP2:
		.byte 3	
.org $0280
MUL24_Result:
		.byte 6	

.org $0290
MUL32_OP1:
		.byte 4	
MUL32_OP2:
		.byte 4	

.org	$02A0	
MUL32_Result:
		.byte 8	

.org $02B0
MUL16_OP1:
		.byte 2	
MUL16_OP2:
		.byte 2	

.org	$02C0	
MUL16_Result:
		.byte 4

.org $02D0
DIV64_OP1:
		.byte 8	
DIV64_OP2:
		.byte 8

.org	$02E0
DIV64_Result:
		.byte 8	

.org $02F0
DIV64_Rem:
		.byte 8
.org $03f0
SQRT_64_CNT:				
		.byte 8
.org	$0260
SQRT_64_VAL:				
		.byte 8

;
;	Allocating Memory used for operands and partial values
;	Please no not modify any of these values above
;


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
INIT:	; The initialization routine
		
		ldi	mpr, low(RAMEND)
		out	SPL, mpr			; Load SPL with low byte of RAMEND
		ldi	mpr, high(RAMEND)
		out	SPH, mpr			; Load SPH with high byte of RAMEND
								; Init the 2 stack pointer registers
		clr		zero

		;load compare value for GM into volatile mem.
		ldi ZL, low(MINGM<<1)
		ldi ZH, high(MINGM<<1)
		ldi XH, high(COMPARE_GM)
		ldi XL, low(COMPARE_GM)
		lpm mpr, Z+
		st X+, mpr
		lpm mpr, Z+
		st X+, mpr
		clr mpr
		st X, mpr

		;load compare value for Radius into volatile mem.
		ldi ZL, low(MINRAD<<1)
		ldi ZH, high(MINRAD<<1)
		ldi XH, high(COMPARE_RADIUS)
		ldi XL, low(COMPARE_RADIUS)
		lpm mpr, Z+
		st X+, mpr
		lpm mpr, Z+
		st X+, mpr

		;load compare value for Period into volatile mem.
		ldi ZL, low(MINPER<<1)
		ldi ZH, high(MINPER<<1)
		ldi XH, high(COMPARE_PERIOD)
		ldi XL, low(COMPARE_PERIOD)
		lpm mpr, Z+
		st X+, mpr
		lpm mpr, Z+
		st X+, mpr
		clr mpr
		st X, mpr

		ldi XH, high(COMPARE_V)
		ldi XL, low(COMPARE_V)
		ldi mpr, 1
		st X+, mpr
		clr mpr
		st X+, mpr


MAIN:
	
	;loading value of "selected planet" (Index)
	ldi ZH, high(SelectedPlanet<<1)
	ldi ZL, low(SelectedPlanet<<1)

	lpm r20, Z

	;
	;	Loading selected planet GM into memory location "PlanetGM"
	;

	ldi ZH, high(PlanetInfo<<1)
	ldi ZL, low(PlanetInfo<<1)

	;loop to get address of selected planet
	planet_selector_loop_BEG:
	cpi r20, $00
	breq planet_selector_loop_END
	adiw Z, 4
	dec r20
	rjmp planet_selector_loop_BEG
	planet_selector_loop_END:
	;load GM of selected planet to memory
	ldi XH, high(PlanetGM)
	ldi XL, low(PlanetGM) 

	lpm mpr, Z+
	st X+, mpr
	lpm mpr, Z+
	st X+, mpr
	lpm mpr, Z+
	st X+, mpr
	lpm mpr, Z+
	st X+, mpr

	;load Orbital Radius to memory
	ldi ZH, high(OrbitalRadius<<1)
	ldi ZL, low(OrbitalRadius<<1)
	ldi XH, high(Radius)
	ldi XL, low(Radius)

	lpm mpr, Z+
	st X+, mpr
	lpm mpr, Z+
	st X+, mpr

	;
	; Now checking if Radius is less than 1000
	;

	ldi XH, high(Radius)
	ldi XL, low(Radius)
	ldi YH, high(COMPARE_RADIUS)
	ldi YL, low(COMPARE_RADIUS)
	;	Checks if the value in X is less than the value in Y (16-bits)
	;	It will set the carry flag if X is less than Y, o.w. cleared
	call COMPARE_16
	brcc main_continue

	ldi mpr, $FF
	ldi XL, low(Velocity)
	ldi XH, high(Velocity)

	st X+, mpr
	st X+, mpr
	
	jmp	Grading

	main_continue:

	;
	; Now comparing if GM is less than 1001
	;

	ldi XH, high(PlanetGM)
	ldi XL, low(PlanetGM)
	ldi YH, high(COMPARE_GM)
	ldi YL, low(COMPARE_GM)
	;	Checks if the value in X is less than the value in Y (32-bits)
	;	It will set the carry flag if X is less than Y, o.w. cleared
	call COMPARE_32
	brcc main_continue2

	ldi mpr, $FF
	ldi XL, low(Period)
	ldi XH, high(Period)

	st X+, mpr
	st X+, mpr
	st X+, mpr
	
	jmp	Grading
	
	main_continue2:

	;At this point, all values are loaded to memory

	;
	;	Loading values for Velocity Calculation
	;

	;Loading GM into Division
	ldi XH, high(PlanetGM)
	ldi XL, low(PlanetGM)
	ldi YH, high(DIV32_OP1)
	ldi YL, low(DIV32_OP1)

	ld mpr, X+
	st Y+, mpr
	ld mpr, X+
	st Y+, mpr
	ld mpr, X+
	st Y+, mpr
	ld mpr, X+
	st Y+, mpr

	;Loading Radius into Division
	ldi XH, high(Radius)
	ldi XL, low(Radius)
	ldi YH, high(DIV32_OP2)
	ldi YL, low(DIV32_OP2)

	ld mpr, X+
	st Y+, mpr
	ld mpr, X+
	st Y+, mpr

	;call division
	call DIV32
	
	ldi XH, high(Quotient)
	ldi XL, low(Quotient)
	ldi YH, high(DIV32_Result)
	ldi YL, low(DIV32_Result)

	ld mpr, Y+
	st X+, mpr
	ld mpr, Y+
	st X+, mpr
	ld mpr, Y+
	st X+, mpr
	
	ldi XH, high(Quotient)
	ldi XL, low(Quotient)
	ldi YH, high(SQRT_32_VAL)
	ldi YL, low(SQRT_32_VAL)

	ld mpr, X+
	st Y+, mpr
	ld mpr, X+
	st Y+, mpr
	ld mpr, X+
	st Y+, mpr
	;ld mpr, X+
	;st Y+, mpr

	call sqrt_32

	;
	;	Check to see if velocity value is 0
	;

	ldi XH, high(Velocity)
	ldi XL, low(Velocity)
	ldi YH, high(COMPARE_V)
	ldi YL, low(COMPARE_V)

	call COMPARE_16
	;	Checks if the value in X is less than the value in Y (16-bits)
	;	It will set the carry flag if X is less than Y, o.w. cleared
	brcc main_velocity_not_0

	;velocity is 0, load -2 to velocity value
	ldi mpr, $FE
	st X+, mpr
	ldi mpr, $FF
	st X, mpr

	;end program
	jmp Grading

	main_velocity_not_0:

	;
	;	Loading Values for Period Calculation (Product Calculation)
	;

	;load Radius to MUL16 OP1 and OP2
	ldi XH, high(Radius)
	ldi XL, low(Radius)
	ldi YL, low(MUL16_OP1)
	ldi YH, high(MUL16_OP1)

	ld mpr, X+
	st Y+, mpr
	ld mpr, X+
	st Y+, mpr

	ldi YL, low(MUL16_OP2)
	ldi YH, high(MUL16_OP2)
	ldi XH, high(Radius)
	ldi XL, low(Radius)

	ld mpr, X+
	st Y+, mpr
	ld mpr, X+
	st Y+, mpr

	;calculate Radius^2
	call MUL16

	;get first result from MUL16 (Radius^2) into OP1 of MUL32

	ldi YL, low(MUL32_OP1)
	ldi YH, high(MUL32_OP1)
	ldi XH, high(MUL16_Result)
	ldi XL, low(MUL16_Result)

	ld mpr, X+
	st Y+, mpr
	ld mpr, X+
	st Y+, mpr
	ld mpr, X+
	st Y+, mpr
	ld mpr, X+
	st Y+, mpr

	;clear results for multiplication
	ldi XH, high(MUL16_Result)
	ldi XL, low(MUL16_Result)
	clr mpr
	st X+, mpr
	st X+, mpr
	st X+, mpr
	st X+, mpr

	;load Radius and 40 to MUL16 (Radius * 4 * 10)
	ldi XH, high(Radius)
	ldi XL, low(Radius)
	ldi YL, low(MUL16_OP1)
	ldi YH, high(MUL16_OP1)

	ld mpr, X+
	st Y+, mpr
	ld mpr, X+
	st Y+, mpr

	ldi YL, low(MUL16_OP2)
	ldi YH, high(MUL16_OP2)

	ldi mpr, $28
	st Y+, mpr
	clr mpr
	st Y+, mpr

	;Calculate Radius * 40
	call MUL16

	;get second result from MUL16 into OP2 of MUL32

	ldi YL, low(MUL32_OP2)
	ldi YH, high(MUL32_OP2)
	ldi XH, high(MUL16_Result)
	ldi XL, low(MUL16_Result)

	ld mpr, X+
	st Y+, mpr
	ld mpr, X+
	st Y+, mpr
	ld mpr, X+
	st Y+, mpr
	ld mpr, X+
	st Y+, mpr

	;Calculate 56-bit product using MUL32 (32*32-bit multiplication)
	call MUL32

	;
	;	Loading Values for Period Calculation (Quotient Calculation)
	;

	ldi XH, high(Product)
	ldi XL, low(Product)
	
	ldi ZH, high(DIV64_OP1)
	ldi ZL, low(DIV64_OP1)

	ld mpr, X+
	st Z+, mpr
	ld mpr, X+
	st Z+, mpr
	ld mpr, X+
	st Z+, mpr
	ld mpr, X+
	st Z+, mpr
	ld mpr, X+
	st Z+, mpr
	ld mpr, X+
	st Z+, mpr
	ld mpr, X+
	st Z+, mpr

	ldi ZH, high(DIV64_OP2)
	ldi ZL, low(DIV64_OP2)
	ldi XH, high(PlanetGM)
	ldi XL, low(PlanetGM)
	
	ld mpr, X+
	st Z+, mpr
	ld mpr, X+
	st Z+, mpr
	ld mpr, X+
	st Z+, mpr
	ld mpr, X+
	st Z+, mpr
	clr mpr
	st Z+, mpr
	st Z+, mpr
	st Z+, mpr

	;calculate quotient
	call DIV64

	;
	;	Loading values for squared root of product/GM
	;

	;get value from quotient and load into squared root
	ldi XH, high(DIV64_Result)
	ldi XL, low(DIV64_Result)
	ldi YH, high(SQRT_64_VAL)
	ldi YL, low(SQRT_64_VAL)

	ld mpr, X+
	st Y+, mpr
	ld mpr, X+
	st Y+, mpr
	ld mpr, X+
	st Y+, mpr
	ld mpr, X+
	st Y+, mpr
	ld mpr, X+
	st Y+, mpr
	ld mpr, X+
	st Y+, mpr
	ld mpr, X+
	st Y+, mpr
	ld mpr, X+
	st Y+, mpr

	call sqrt_64

	;
	;	Check to see if period is less than 25s
	;

	ldi XH, high(SQRT_64_CNT)
	ldi XL, low(SQRT_64_CNT)
	ldi YH, high(COMPARE_PERIOD)
	ldi YL, low(COMPARE_PERIOD)

	;	Checks if the value in X is less than the value in Y (24-bits)
	;	It will set the carry flag if X is less than Y, o.w. cleared
	call COMPARE_24
	brcc P_greater_25

	;store -2 to Period
	ldi mpr, $FE
	ldi XL, low(Period)
	ldi XH, high(Period)

	st X+, mpr
	ldi mpr, $FF
	st X+, mpr
	st X+, mpr
	
	jmp	Grading
	P_greater_25:

	jmp	Grading				; this should be the very last instruction of your code

;-----------------------------------------------------------
;	Procedures and Subroutines
;-----------------------------------------------------------

;
;	BEGINNING OF PART 1 PROCEDURES
;

;
;	DIV32
;	Divides DIV32_OP1 by DIV32_OP2, essentially 32-bit divided by 16-bit
;	Outputs result to DIV32_Result after rounding
;	based on algorithm found @ http://www.rjhcoding.com/avr-asm-8bit-division.php
;	for 8-bit division, and adapted for 32 / 16 bits.
;

DIV32:
	push XH
	push XL
	push YH
	push YL
	push ZH
	push ZL

	push r0 ;ans
	push r1
	push r2
	push r3
	
	push r8 ;rem
	push r9
	push r10
	push r11
	
	push r16 ;divisor
	push r17
	
	push r18
	push r19

	push r21
	push r22

        clr ZERO

		clr r0
		clr r1
		clr r2
		clr r3
		clr r8
		clr r9
		clr r10
		clr r11
		clr r16
		clr r17
		clr r18
		clr r19
		clr r21
		clr r22
        
		;copy dividend to answer, r0:r3
		ldi XH, high(DIV32_OP1)
		ldi XL, low(DIV32_OP1)
		ld r0, X+
		ld r1, X+
		ld r2, X+
		ld r3, X+

		;loading divisor to registers
		ldi XH, high(DIV32_OP2)
		ldi XL, low(DIV32_OP2)
		ld r16, X+
		ld r17, X+

restart_32:
		
		;Load bit counter for 33 iterations (32-bits + carry)
        ldi r22,33

		;Clear Remainder and Carry
        sub r8,r8 
        clr r9
        clr r10
        clr r11

LOOP32:   
		;Shift the answer to the left
		rol r0         
        rol r1          
        rol r2          
        rol r3         
        
		;Decrement Counter
		dec r22
		
		;Exit if 32 bits done
        breq DONE32
        
		;Shift remainder to the left
		rol r8          
        rol r9          
        rol r10          
        rol r11          

		;Try to subtract divisor from remainder
        sub r8,r16
        sbc r9,r17       
        sbc r10,ZERO     
        sbc r11,ZERO     
        
		;If the result was negative then
		;reverse the subtraction to divide again
		brcc SKIP32  
        
		add r8,r16 
        adc r9,r17       
        adc r10,ZERO 
        adc r11,ZERO

		;Clear Carry Flag so zero shifted into result
        clc             
        
		RJMP LOOP32        ;Loop Back
SKIP32:   
		;Set Carry Flag to be shifted into result
		SEC               
         
		RJMP LOOP32
DONE32:
	;get remainder, and result and save to memory (increment the value already there)
	;check if remainder is greater than divisor, if it is, divide again, else, return

	;add result to the result stored in memory
	ldi XH, high(DIV32_Result)
	ldi XL, low(DIV32_Result)
	
	ld r21, X
	add r21, r0
	st X+, r21
	ld r21, X
	adc r21, r1
	st X+, r21
	ld r21, X
	adc r21, r2
	st X+, r21
	ld r21, X
	adc r21, r3
	st X+, r21

	;update the remainder
	ldi XH, high(DIV32_Rem)
	ldi XL, low(DIV32_Rem)
	st X+, r8
	st X+, r9
	st X+, r10
	st X+, r11

	;prepare to compare remainder and divisor
	ldi XH, high(DIV32_Rem)
	ldi XL, low(DIV32_Rem)
	ldi YH, high(DIV32_OP2)
	ldi YL, low(DIV32_OP2)

	;	Checks if the value in X is less than the value in Y (64-bits)
	;	It will set the carry flag if X is less than Y, o.w. cleared
	call COMPARE_32
	brcs get_div32_remainder
	breq get_div32_remainder

	;the remainder is still greater than the divisor

	;load remainder into result
	mov r0, r8
	mov r1, r9
	mov r2, r10
	mov r3, r11

	clr r8
	clr r9
	clr r10
	clr r11

	;divide again
	jmp restart_32

	get_div32_remainder:

	;divide divisor by 2
	lsr r17
	ror r16

	ldi YH, high(DIV32_OP2)
	ldi YL, low(DIV32_OP2)
	st Y+, r16
	st Y+, r17

	;check if remainder is less than half of the divisor	
	ldi XH, high(DIV32_Rem)
	ldi XL, low(DIV32_Rem)
	ldi YH, high(DIV32_OP2)
	ldi YL, low(DIV32_OP2)

	;	Checks if the value in X is less than the value in Y (64-bits)
	;	It will set the carry flag if X is less than Y, o.w. cleared
	call COMPARE_32
	brcs return_32

	clr mpr
	clr r19
	ldi XH, high(DIV32_Result)
	ldi XL, low(DIV32_Result)
	
	ldi r19, 1
	
	ld mpr, X
	add mpr, r19
	st X+, mpr
	ld mpr, X
	adc mpr, zero
	st X+, mpr
	ld mpr, X
	adc mpr, zero
	st X+, mpr
	ld mpr, X
	adc mpr, zero
	st X+, mpr

	return_32:
	pop r22
	pop r21
	pop r19
	pop r18
	pop r17
	pop r16
	pop r11
	pop r10
	pop r9
	pop r8
	pop r3
	pop r2
	pop r1
	pop r0
	pop ZL
	pop ZH
	pop YL
	pop YH
	pop XL
	pop XH
	ret

;
;	clear_ops_32
;	clears operands in X and Y for 32-bits
;
clear_ops_32:
	push mpr
	push XL
	push XH
	push YH
	push YL

	clr mpr
	st X+, mpr
	st X+, mpr
	st X+, mpr
	st X+, mpr
	st Y+, mpr
	st Y+, mpr
	st Y+, mpr
	st Y+, mpr

	pop YL
	pop YH
	pop XH
	pop XL
	pop mpr
	ret

;
;	COMPARE32
;	Checks if the value in X is less than the value in Y (32-bits)
;	It will set the carry flag if X is less than Y, o.w. cleared
;
COMPARE_32:
	push XH
	push XL
	push YH
	push YL
	push ZH
	push ZL
	push mpr
	push r19

	ld mpr, X+
	ld r19, Y+
	cp mpr, r19
	ld mpr, X+
	ld r19, Y+
	cpc mpr, r19
	ld mpr, X+
	ld r19, Y+
	cpc mpr, r19
	ld mpr, X+
	ld r19, Y+
	cpc mpr, r19

	;carry flag cleared means that the result is larger than the divisor

	pop r19
	pop mpr
	pop ZL
	pop ZH
	pop YL
	pop YH
	pop XL
	pop XH
	ret

;
;	COMPARE24
;	Checks if the value in X is less than the value in Y (24-bits)
;	It will set the carry flag if X is less than Y, o.w. cleared
;
COMPARE_24:
	push XH
	push XL
	push YH
	push YL
	push ZH
	push ZL
	push mpr
	push r19

	ld mpr, X+
	ld r19, Y+
	cp mpr, r19
	ld mpr, X+
	ld r19, Y+
	cpc mpr, r19
	ld mpr, X+
	ld r19, Y+
	cpc mpr, r19

	;carry flag cleared means that the result is larger than the divisor

	pop r19
	pop mpr
	pop ZL
	pop ZH
	pop YL
	pop YH
	pop XL
	pop XH
	ret
;
;	COMPARE16
;	Checks if the value in X is less than the value in Y (16-bits)
;	It will set the carry flag if X is less than Y, o.w. cleared
;
COMPARE_16:
	push XH
	push XL
	push YH
	push YL
	push mpr
	push r19

	ld mpr, X+
	ld r19, Y+
	cp mpr, r19
	ld mpr, X+
	ld r19, Y+
	cpc mpr, r19

	;carry flag cleared means that the result is larger than the divisor

	pop r19
	pop mpr
	pop YL
	pop YH
	pop XL
	pop XH
	ret

;
;	sqrt_32
;	Calculates the squared root of SQRT_32_VAL
;	for 32-bit values (returns up to 24-bits)
;
sqrt_32:
	push mpr
	push r18
	push r17

	clr mpr
	clr r18
	clr r17


	sqrt_32_BEG:
	;increment 24-bit number
	call INC_24_sqrt_32

	ldi XH, high(MUL24_OP2)
	ldi XL, low(MUL24_OP2)
	ldi YH, high(MUL24_OP1)
	ldi YL, low(MUL24_OP1)
	ldi ZH, high(SQRT_32_CNT)
	ldi ZL, low(SQRT_32_CNT)
	call clear_ops_32

	ld mpr, Z+
	st Y+, mpr
	st X+, mpr
	ld mpr, Z+
	st Y+, mpr
	st X+, mpr
	ld mpr, Z+
	st Y+, mpr
	st X+, mpr

	push mpr
	ldi ZH, high(MUL24_Result)
	ldi ZL, low(MUL24_Result)
	clr mpr
	st Z+, mpr
	st Z+, mpr
	st Z+, mpr
	st Z+, mpr
	st Z+, mpr
	st Z+, mpr
	pop mpr

	;square the number
	call MUL24

	;check if X is less than Y
	ldi XH, high(MUL24_Result)
	ldi XL, low(MUL24_Result)
	ldi YH, high(SQRT_32_VAL)
	ldi YL, low(SQRT_32_VAL)
	
	;	Checks if the value in X is less than the value in Y (32-bits)
	;	It will set the carry flag if X is less than Y, o.w. cleared
	call COMPARE_32
	brcs sqrt_32_BEG

	;here the count is one too large, subtract 1 from count
	ldi XH, high(SQRT_32_CNT)
	ldi XL, low(SQRT_32_CNT)

	ldi ZH, high(Velocity)
	ldi ZL, low(Velocity)
	
	ld r17, X+	; Load (X) to R17
	ldi r18, 1
	sub R17,R18 ; Rsubtract
	st Z+, R17	; store result to (Z)

	ld r17, X+	; Load (X) to R17
	clr r18
	sbc R17,R18 ; Rsubtract
	st Z+, R17	; store result to (Z)

	ld r17, X+	; Load (X) to R17
	sbc R17,R18 ; Rsubtract
	st Z+, R17	; store result to (Z)
	sqrt_32_END:

	pop r17
	pop r18
	pop mpr
	RET

;
; INC_24_sqrt_32
; Increments 24-bit number for squared root of a 32-bit value
;
INC_24_sqrt_32:
	push mpr
	push r19
	
	ldi XH, high(SQRT_32_CNT)
	ldi XL, low(SQRT_32_CNT)
	ldi r19, 1

	ld mpr, X
	add mpr, r19
	st X+, mpr
	clr r19

	ld mpr, X
	adc mpr, r19
	st X+, mpr
	
	ld mpr, X
	adc mpr, r19
	st X+, mpr

	pop r19
	pop mpr
	ret

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

;
;	NO MORE PROCEDURES FOR PART 1 BELOW THIS POINT
;	BEGINNING OF PART 2 PROCEDURES
;

;
;	DIV64
;	Divides DIV64_OP1 by DIV64_OP2 (OP1 up to 64-bits, OP2 up to 32-bits)
;	Outputs result to DIV64_Result after rounding
;	based on algorithm found @ http://www.rjhcoding.com/avr-asm-8bit-division.php
;	for 8-bit division, and adapted for 64 / 32 bits.
;
DIV64:
	push XH
	push XL
	push YH
	push YL
	push ZH
	push ZL

	push r0 ;ans
	push r1
	push r2
	push r3
	push r4
	push r5
	push r6
	push r7 
	
	push r8 ;rem
	push r9
	push r10
	push r11
	push r12
	push r13
	push r14
	push r15
	
	push r16 ;divisor
	push r17
	push r18
	push r19

	push r21
	push r22

        clr ZERO

		;clear all registers that are going to be used
		clr r0
		clr r1
		clr r2
		clr r3
		clr r4
		clr r5
		clr r6
		clr r7
		clr r8
		clr r9
		clr r10
		clr r11
		clr r12
		clr r13
		clr r14
		clr r15
		clr r16
		clr r17
		clr r18
		clr r21
		clr r22
        ;copy dividend to answer, r0:r7

		ldi XH, high(DIV64_OP1)
		ldi XL, low(DIV64_OP1)
		ld r0, X+
		ld r1, X+
		ld r2, X+
		ld r3, X+
		ld r4, X+
		ld r5, X+
		ld r6, X+
		ld r7, X+

		;loading divisor to registers
		ldi XH, high(DIV64_OP2)
		ldi XL, low(DIV64_OP2)
		ld r16, X+
		ld r17, X+
		ld r18, X+
		ld r19, X+

restart_64:
        ldi r22,65          ;Load bit counter for 64-bits
        sub r8,r8		  ;Clear Remainder and Carry
        clr r9
        clr r10        
        clr r11       
		clr r12
		clr r13
		clr r14
		clr r15

LOOP_64:   rol r0          ;Shift the answer to the left
        rol r1          
        rol r2          
        rol r3          
		rol r4
		rol r5
		rol r6
		rol r7

		;decrement counter
        dec r22     
        breq DONE_64        ;Exit loop if 64 bits are done

        rol r8				;Shift remainder to the left
        rol r9			
        rol r10         
        rol r11         
		rol r12
		rol r13
		rol r14
		rol r15

        sub r8, r16       ;subtract divisor from remainder
		sbc r9, r17
		sbc r10, r18
		sbc r11, r19
		sbc r12, zero
		sbc r13, zero
		sbc r14, zero
		sbc r15, zero

        BRCC SKIP_64        ;If the result was negative
        
		add r8, r16			;go back and undo subtraction
		adc r9, r17
		adc r10, r18
		adc r11, r19
		adc r12, zero
		adc r13, zero
		adc r14, zero
		adc r15, zero

        CLC					 ;Clear Carry Flag so zero shifted into result 
         RJMP LOOP_64        ;Loop Back
SKIP_64:   SEC               ;Set Carry Flag to be shifted into result
         RJMP LOOP_64		 ;loop back
DONE_64:

	;get remainder, and result and save to memory (increment the value already there)
	;check if remainder is greater than divisor, if it is, divide again, else, return

	;add result to the result stored in memory
	ldi XH, high(DIV64_Result)
	ldi XL, low(DIV64_Result)
	
	ld r21, X
	add r21, r0
	st X+, r21
	ld r21, X
	adc r21, r1
	st X+, r21
	ld r21, X
	adc r21, r2
	st X+, r21
	ld r21, X
	adc r21, r3
	st X+, r21
	ld r21, X
	adc r21, r4
	st X+, r21
	ld r21, X
	adc r21, r5
	st X+, r21
	ld r21, X
	adc r21, r6
	st X+, r21
	ld r21, X
	adc r21, r7
	st X+, r21

	;update remainder in memory
	ldi XH, high(DIV64_Rem)
	ldi XL, low(DIV64_Rem)
	st X+, r8
	st X+, r9
	st X+, r10
	st X+, r11
	st X+, r12
	st X+, r13
	st X+, r14
	st X+, r15

	ldi XH, high(DIV64_Rem)
	ldi XL, low(DIV64_Rem)
	ldi YH, high(DIV64_OP2)
	ldi YL, low(DIV64_OP2)

	;	Checks if the value in X is less than the value in Y (64-bits)
	;	It will set the carry flag if X is less than Y, o.w. cleared
	call COMPARE_64
	brcs get_div64_remainder
	breq get_div64_remainder

	;the remainder is still greater than the divisor

	;load remainder into result
	mov r0, r8
	mov r1, r9
	mov r2, r10
	mov r3, r11
	mov r4, r12
	mov r5, r13
	mov r6, r14
	mov r7, r15

	clr r8
	clr r9
	clr r10
	clr r11
	clr r12
	clr r13
	clr r14
	clr r15
	jmp restart_64

	get_div64_remainder:

	;divide divisor by 2
	lsr r19
	ror r18
	ror r17
	ror r16

	ldi YH, high(DIV64_OP2)
	ldi YL, low(DIV64_OP2)
	st Y+, r16
	st Y+, r17
	st Y+, r18
	st Y+, r19

	;check if remainder is less than half of the divisor	
	ldi XH, high(DIV64_Rem)
	ldi XL, low(DIV64_Rem)
	ldi YH, high(DIV64_OP2)
	ldi YL, low(DIV64_OP2)

	;	Checks if the value in X is less than the value in Y (64-bits)
	;	It will set the carry flag if X is less than Y, o.w. cleared
	call COMPARE_64
	brcs return_64

	;clear registers to be used
	clr mpr
	clr r19
	clr zero

	ldi XH, high(DIV64_Result)
	ldi XL, low(DIV64_Result)
	
	ldi r19, 1
	ld mpr, X
	add mpr, r19
	st X+, mpr
	ld mpr, X
	adc mpr, zero
	st X+, mpr
	ld mpr, X
	adc mpr, zero
	st X+, mpr
	ld mpr, X
	adc mpr, zero
	st X+, mpr
	ld mpr, X
	adc mpr, zero
	st X+, mpr
	ld mpr, X
	adc mpr, zero
	st X+, mpr

	return_64:
	pop r22
	pop r21
	pop r19
	pop r18
	pop r17
	pop r16
	pop r15
	pop r14
	pop r13
	pop r12
	pop r11
	pop r10
	pop r9
	pop r8
	pop r7
	pop r6
	pop r5
	pop r4
	pop r3
	pop r2
	pop r1
	pop r0
	pop ZL
	pop ZH
	pop YL
	pop YH
	pop XL
	pop XH
	ret


;-----------------------------------------------------------
; Func: MUL32
; Desc: Multiplies two 32-bit numbers and generates a 64-bit 
;		result.
;-----------------------------------------------------------
MUL32:
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
		push	r20				

		clr		zero			; Maintain zero semantics

		; Set Y to beginning address of B
		ldi		YL, low(MUL32_OP1)	; Load low byte
		ldi		YH, high(MUL32_OP1)	; Load high byte

		; Set Z to begginning address of resulting Product
		ldi		ZL, low(Product)	; Load low byte
		ldi		ZH, high(Product); Load high byte

		; Begin outer for loop
		ldi		oloop, 4		; Load counter
MUL32_OLOOP:
		; Set X to beginning address of A
		ldi		XL, low(MUL32_OP2)	; Load low byte
		ldi		XH, high(MUL32_OP2)	; Load high byte

		; Begin inner for loop
		ldi		iloop, 4		; Load counter
MUL32_ILOOP:
		ld		A, X+			; Get byte of A operand
		ld		B, Y			; Get byte of B operand
		mul		A,B				; Multiply A and B
		ld		A, Z+			; Get a result byte from memory
		ld		B, Z+			; Get the next result byte from memory
		add		rlo, A			; rlo <= rlo + A
		adc		rhi, B			; rhi <= rhi + B + carry
		ld		A, Z+			; Get a third byte from the result
		ld		R19, Z+			; Get the 4th byte
		ld		R20, Z			; Get the 5th byte
		adc		A, zero			; Add carry to A
		adc		R19, zero		; Add carry to R19
		adc		R20, zero
		st		Z, R20			; Store fith byte to memory
		st		-Z, R19			; Store fourth byte to memory
		st		-Z, A			; Store third byte to memory
		st		-Z, rhi			; Store second byte to memory
		st		-Z, rlo			; Store first byte to memory
		adiw	ZH:ZL, 1		; Z <= Z + 1			
		dec		iloop			; Decrement counter
		brne	MUL32_ILOOP		; Loop if iLoop != 0
		; End inner for loop

		sbiw	ZH:ZL, 3		; Z <= Z - 3
		adiw	YH:YL, 1		; Y <= Y + 1
		dec		oloop			; Decrement counter
		brne	MUL32_OLOOP		; Loop if oLoop != 0
		; End outer for loop
		 	
		pop		r20		
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

;-----------------------------------------------------------
; Func: MUL16
; Desc: An example function that multiplies two 16-bit numbers
;			A - Operand A
;			B - Operand B
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
		ldi		YL, low(MUL16_OP1)	; Load low byte
		ldi		YH, high(MUL16_OP1)	; Load high byte

		; Set Z to beginning address of resulting Product
		ldi		ZL, low(MUL16_Result)	; Load low byte
		ldi		ZH, high(MUL16_Result); Load high byte

		; Begin outer for loop
		ldi		oloop, 2		; Load counter
MUL16_OLOOP:
		; Set X to beginning address of A
		ldi		XL, low(MUL16_OP2)	; Load low byte
		ldi		XH, high(MUL16_OP2)	; Load high byte

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

;
;	COMPARE64
;	Checks if the value in X is less than the value in Y (64-bits)
;	It will set the carry flag if X is less than Y, o.w. cleared
;
COMPARE_64:
	push XH
	push XL
	push YH
	push YL
	push ZH
	push ZL
	push mpr
	push r19

	ld mpr, X+
	ld r19, Y+
	cp mpr, r19
	ld mpr, X+
	ld r19, Y+
	cpc mpr, r19
	ld mpr, X+
	ld r19, Y+
	cpc mpr, r19
	ld mpr, X+
	ld r19, Y+
	cpc mpr, r19
	
	ld mpr, X+
	ld r19, Y+
	cpc mpr, r19
	ld mpr, X+
	ld r19, Y+
	cpc mpr, r19
	ld mpr, X+
	ld r19, Y+
	cpc mpr, r19
	ld mpr, X+
	ld r19, Y+
	cpc mpr, r19

	;carry flag cleared means that the result is larger than the divisor

	pop r19
	pop mpr
	pop ZL
	pop ZH
	pop YL
	pop YH
	pop XL
	pop XH
	ret

;
; sqrt_64
; Gets squared root for a 64-bit number
;
sqrt_64:
	push mpr
	push r18
	push r17

	clr mpr
	clr r18
	clr r17

	ldi XH, high(MUL24_Result)
	ldi XL, low(MUL24_Result)
	st X+, mpr
	st X+, mpr
	st X+, mpr
	st X+, mpr
	st X+, mpr
	st X+, mpr

	sqrt_64_BEG:
	;increment 24-bit number
	call INC_24_sqrt_64

	ldi XH, high(MUL24_OP2)
	ldi XL, low(MUL24_OP2)
	ldi YH, high(MUL24_OP1)
	ldi YL, low(MUL24_OP1)
	ldi ZH, high(SQRT_64_CNT)
	ldi ZL, low(SQRT_64_CNT)
	call clear_ops_32

	ld mpr, Z+
	st Y+, mpr
	st X+, mpr
	ld mpr, Z+
	st Y+, mpr
	st X+, mpr
	ld mpr, Z+
	st Y+, mpr
	st X+, mpr

	push mpr
	ldi ZH, high(MUL24_Result)
	ldi ZL, low(MUL24_Result)
	clr mpr
	st Z+, mpr
	st Z+, mpr
	st Z+, mpr
	st Z+, mpr
	st Z+, mpr
	st Z+, mpr
	pop mpr

	;square the number
	call MUL24
	
	;compare with value from previous result
	;check if X is less than Y
	ldi XH, high(MUL24_Result)
	ldi XL, low(MUL24_Result)
	ldi YH, high(SQRT_64_VAL)
	ldi YL, low(SQRT_64_VAL)
	
	;	Checks if the value in X is less than the value in Y (32-bits)
	;	It will set the carry flag if X is less than Y, o.w. cleared
	call COMPARE_64
	brcs sqrt_64_BEG

	;here the count is one too large, subtract 1 from count
	ldi XH, high(SQRT_64_CNT)
	ldi XL, low(SQRT_64_CNT)

	ldi ZH, high(Period)
	ldi ZL, low(Period)
	
	ld r17, X+	; Load (X) to R17
	ldi r18, 1
	sub R17,R18 ; Rsubtract
	st Z+, R17	; store result to (Z)

	ld r17, X+	; Load (X) to R17
	clr r18
	sbc R17,R18 ; Rsubtract
	st Z+, R17	; store result to (Z)

	ld r17, X+	; Load (X) to R17
	sbc R17,R18 ; Rsubtract
	st Z+, R17	; store result to (Z)
	sqrt_64_END:

	pop r17
	pop r18
	pop mpr
	RET

;
; INC_24_sqrt_64
; Increses 24-bit count for 64-bit squared root
;
INC_24_sqrt_64:
	push mpr
	push r19
	
	ldi XH, high(SQRT_64_CNT)
	ldi XL, low(SQRT_64_CNT)
	ldi r19, 1

	ld mpr, X
	add mpr, r19
	st X+, mpr
	clr r19

	ld mpr, X
	adc mpr, r19
	st X+, mpr
	
	ld mpr, X
	adc mpr, r19
	st X+, mpr

	pop r19
	pop mpr
	ret

;***********************************************************
;*	Custom stored data
;*	(feel free to edit these or add others)
;***********************************************************
MINGM:	.DB 0xE9, 0x03
MINRAD:	.DB 0xE8, 0x03
MINPER:	.DB	0x19, 0x00



;***end of your code***end of your code***end of your code***end of your code***end of your code***
;*************************** Do not change anything below this point*******************************
;*************************** Do not change anything below this point*******************************
;*************************** Do not change anything below this point*******************************

Grading:
		nop					; Check the results in data memory begining at address $0E00 (The TA will set a breakpoint here)
rjmp Grading


;***********************************************************
;*	Stored program data that you cannot change
;***********************************************************

; Contents of program memory will be changed during testing
; The label names (OrbitalRadius, SelectedPlanet, PlanetInfo, MercuryGM, etc) are not changed
; NOTE: All values are provided using the little-endian convention.
OrbitalRadius:	.DB	0x64, 0x19				; the radius that should be used during computations (in kilometers)
											; in this example, the value is 6,500 kilometers
											; the radius will be provided as a 16 bit unsigned value (unless you are
											; completing the extra credit, in which case the radius is an unsigned 24 bit value)

SelectedPlanet:	.DB	0x02, 0x00				; This is how your program knows which GM value should be used.
											; SelectedPlanet is an unsigned 8 bit value that provides you with the
											; index of the planet (and hence, tells you which GM value to use).
											; Note: only the first byte is used. The second byte is just for padding.
											; In this example, the value is 2. If we check the planet at index 2, (from the data below)
											; that corresponds to Earth.
											; if the value was 7, that would correspond to the planet Neptune

PlanetInfo:									; Note that these values will be changed during testing!
MercuryGM:		.DB	0x0E, 0x56, 0x00, 0x00	; Gravitational parameters will be provided as unsigned 32 bit integers (little-endian)
VenusGM:		.DB	0x24, 0xF5, 0x04, 0x00	; the units are in: (km * km * km)/(sec * sec)
EarthGM:		.DB	0x08, 0x15, 0x06, 0x00	; <-- note that this is 398,600
MarsGM:			.DB	0x4E, 0xA7, 0x00, 0x00
JupiterGM:		.DB	0x30, 0x13, 0x8D, 0x07	; A word of advice... treat these like an array, where each element
SaturnGM:		.DB	0xF8, 0xC7, 0x42, 0x02	; occupies 4 bytes of memory.
UranusGM:		.DB	0xD0, 0x68, 0x58, 0x00	; Mercury is at index 0, Venus is at index 1, ...and the final planet is at index 8.
NeptuneGM:		.DB	0x38, 0x4B, 0x68, 0x00
FinalGM:		.DB	0xFF, 0xFF, 0xFF, 0xFF


;***********************************************************
;*	Data Memory Allocation for Results
;*	Your answers need to be stored into these locations (using little-endian representation)
;*	These exact variable names will be used when testing your code!
;***********************************************************
.dseg
.org	$0E00						; data memory allocation for results - Your grader only checks $0E00 - $0E14
Quotient:		.byte 3				; This is the intermediate value that is generated while you are computing the satellite's velocity.
									; It is a 24 bit unsigned value.
Velocity:		.byte 2				; This is where you will store the computed velocity. It is a 16 bit signed number.
									; The velocity value is normally positive, but it can also be -1 or -2 in case of error
									; (see "Special Cases" in the assignment documentation).
Product:		.byte 7				; This is the intermediate product that is generated while you are computing the orbital period.
Period:			.byte 3				; This is where the orbital period of the satellite will be placed.
									; It is a 24 bit signed value.
									; The period value is normally positive, but it can also be -1 or -2 in case of error
									; (see "Special Cases" in the assignment documentation).

;***********************************************************
;*	Additional Program Includes
;***********************************************************
; There are no additional file includes for this program
