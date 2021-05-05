;***********************************************************
;*
;*	Caden_Verzino_Felipe_OrricoScognamiglio_Lab7_Sourcecode.asm
;*
;*	This program will increase or decrease the speed of the tekbot
;*	by adjusting the timers within the ATmega128, this way creating
;*	a PWM signal that can have 15 different duty cycles. 
;*
;***********************************************************
;*
;*	 Author: Felipe Orrico Scognamiglio & Caden Verzino
;*	   Date: 02/18/2021
;*
;***********************************************************

.include "m128def.inc"			; Include definition file

;***********************************************************
;*	Internal Register Definitions and Constants
;***********************************************************
.def	mpr = r16				; Multipurpose register
.def	spd_r = r17				; current speed register
.def	ledr = r18				; led on/off register
.def	waitcnt = r17			; Wait Loop Counter
.def	ilcnt = r18				; Inner Loop Counter
.def	olcnt = r19				; Outer Loop Counter

.equ	EngEnR = 4				; right Engine Enable Bit
.equ	EngEnL = 7				; left Engine Enable Bit
.equ	EngDirR = 5				; right Engine Direction Bit
.equ	EngDirL = 6				; left Engine Direction Bit
.equ	SPD_Change = 17			; amount of speed changed per click
.equ	WTime = 20				; Time to wait in wait loop

;***********************************************************
;*	Start of Code Segment
;***********************************************************
.cseg							; beginning of code segment

;***********************************************************
;*	Interrupt Vectors
;***********************************************************
.org	$0000
		rjmp	INIT			; reset interrupt

		; place instructions in interrupt vectors here, if needed
.org	$0002				;INT0 Interrupt Vector
		rcall DEC_SPD		;interrupt handler
		reti

.org	$0004				;INT1 interrupt vector
		rcall INC_SPD		;interrupt handler
		reti
			
.org	$0006				;INT2 interrupt vector
		rcall Z_SPD		;interrupt handler
		reti

.org	$0008				;INT3 interrupt vector
		rcall TOP_SPD			;interrupt handler
		reti

.org	$0046					; end of interrupt vectors

;***********************************************************
;*	Program Initialization
;***********************************************************
INIT:
		; Initialize the Stack Pointer
		ldi		mpr, low(RAMEND)
		out		SPL, mpr		; Load SPL with low byte of RAMEND
		ldi		mpr, high(RAMEND)
		out		SPH, mpr		; Load SPH with high byte of RAMEND
		
		; Initialize Port B for output
		ldi		mpr, $FF		; Set Port B Data Direction Register
		out		DDRB, mpr		; for output
		ldi		mpr, $00		; Initialize Port B Data Register
		out		PORTB, mpr		; so all Port B outputs are low		

		; Initialize Port D for input
		ldi		mpr, $00		; Set Port D Data Direction Register
		out		DDRD, mpr		; for input
		ldi		mpr, $FF		; Initialize Port D Data Register
		out		PORTD, mpr		; so all Port D inputs are Tri-State

		; Configure External Interrupts, if needed
								; Set the Interrupt Sense Control to falling edge 
		ldi mpr, (1<<ISC01)|(0<<ISC00)|(1<<ISC11)|(0<<ISC10)|(1<<ISC20)|(0<<ISC21)|(1<<ISC30)|(0<<ISC31)
		sts EICRA, mpr			; Use sts, EICRA is in extended I/O space
		; Set the External Interrupt Mask
		ldi mpr, (1<<INT0)|(1<<INT1)|(1<<INT2)|(1<<INT3) 
		out EIMSK, mpr

		; Configure 8-bit Timer/Counters
		ldi mpr, 0b01101001 ;no prescalar, fast pwm, clear on compare match
		out TCCR0, mpr			;;load clock info to TCCR1
		ldi mpr, 255			;initial speed of 15
		out OCR0, mpr			;load initial speed to OCR1

		ldi mpr, 0b01101001 ;no prescalar, fast pwm, clear on compare match
		out TCCR2, mpr			;load clock info to TCCR2
		ldi mpr, 255			;initial speed of 15
		out OCR2, mpr			;load initial speed to OCR2

		ldi spd_r, 0			;set initial speed of spd_r (speed register) to 0

		; Set TekBot to Move Forward (1<<EngDirR|1<<EngDirL)
		ldi mpr, (1<<EngDirR|1<<EngDirL); Load FWD command
		out PORTB, mpr; Send to motors

		; Set initial speed, display on Port B pins 3:0
		ldi ledr, (1<<6)|(1<<5) ; set bit 5/6 to 1 

		; Enable global interrupts (if any are used)
		sei	; NOTE: This must be the last thing to do in the INIT function

;***********************************************************
;*	Main Program (does nothing)
;***********************************************************
MAIN:

		rjmp	MAIN			; return to top of MAIN

;***********************************************************
;*	Functions and Subroutines
;***********************************************************


;-----------------------------------------------------------
; Func:	INC_SPD
; Desc:	Increase the speed of the robot by decreasing the
;		duty cycle of the PWM
;-----------------------------------------------------------
INC_SPD:
		rcall Wait2	;wait for stabilization of input

		;load current duty cycle to mpr and compare to full speed (0)
		in mpr, OCR0
		cpi mpr, 0
		breq Skip_sub ;if the current duty cycle is 0 (max speed) skip

		;subtract the delta from the duty cycle and store that back
		ldi r19, SPD_Change
		sub mpr, r19
		out OCR0, mpr
		out OCR2, mpr

		;increment the leds to display increasing
		inc ledr
		out PORTB, ledr
		inc spd_r

		Skip_sub:
		;clear interrupts
		ldi mpr,0b00001111
		out EIFR, mpr

	ret

;-----------------------------------------------------------
; Func:	DEC_SPD
; Desc:	Decrease the speed of the robot by increasing the
;		duty cycle of the PWM
;-----------------------------------------------------------
DEC_SPD:
		rcall Wait2	;wait for stabilization of input

		;load current duty cycle to mpr and compare to 0 speed (255)
		in mpr, OCR0
		cpi mpr, 255
		breq Skip_add ;if the current duty cycle is 255 (min speed) skip

		;sadd the delta to the duty cycle and store that back
		ldi r19, SPD_Change
		add mpr, r19
		out OCR0, mpr
		out OCR2, mpr

		;decrement the leds to display decreasing
		dec spd_r
		dec ledr
		out PORTB, ledr

		Skip_add:
		;clear interrupts
		ldi mpr,0b00001111
		out EIFR, mpr

	ret

;-----------------------------------------------------------
; Func:	TOP_SPD
; Desc:	Set the speed of the robot to max by setting the
;		duty cycle of the PWM to 0
;-----------------------------------------------------------
TOP_SPD:
	rcall Wait2	;wait for stabilization of input

	;set duty cycle to 0 (max speed)
	ldi r19, $00
	out OCR0, r19
	out OCR2, r19

	;set leds 5,6,0,1,2,3 to ON
	ldi ledr, (1<<6)|(1<<5)|(1<<0)|(1<<1)|(1<<2)|(1<<3)
	out PORTB, ledr
	
	;set speed to speed register to 15
	ldi r19, $0F
	mov spd_r, r19

	;clear interrupts
	ldi mpr,0b00001111
	out EIFR, mpr
	ret

;-----------------------------------------------------------
; Func:	Z_SPD
; Desc:	Set the speed of the robot to min by setting the
;		duty cycle of the PWM to 255
;-----------------------------------------------------------
Z_SPD:
	rcall Wait2	;wait for stabilization of input

	;set duty cycle to 255 (min speed)
	ldi r19, $FF
	out OCR0, r19
	out OCR2, r19

	//set leds 5,6,4,7 to ON
	ldi ledr, (1<<6)|(1<<5)|(1<<4)|(1<<7)
	out PORTB, ledr
	
	;set speed to speed register to 0
	ldi r19, $00
	mov spd_r, r19

	;clear interrupts
	ldi mpr,0b00001111
	out EIFR, mpr
	ret

;----------------------------------------------------------------
; Sub:	Wait
; Desc:	A wait loop that is 16 + 159975*waitcnt cycles or roughly 
;		waitcnt*10ms.  Just initialize wait for the specific amount 
;		of time in 10ms intervals. Here is the general eqaution
;		for the number of clock cycles in the wait loop:
;			((3 * ilcnt + 3) * olcnt + 3) * waitcnt + 13 + call
;----------------------------------------------------------------
Wait2:
		push	waitcnt			; Save wait register
		push	ilcnt			; Save ilcnt register
		push	olcnt			; Save olcnt register

		ldi waitcnt, WTime		; Load time to delay 

Loop:	ldi		olcnt, 224		; load olcnt register
OLoop:	ldi		ilcnt, 237		; load ilcnt register
ILoop:	dec		ilcnt			; decrement ilcnt
		brne	ILoop			; Continue Inner Loop
		dec		olcnt		; decrement olcnt
		brne	OLoop			; Continue Outer Loop
		dec		waitcnt		; Decrement wait 
		brne	Loop			; Continue Wait loop	

		pop		olcnt		; Restore olcnt register
		pop		ilcnt		; Restore ilcnt register
		pop		waitcnt		; Restore wait register
		ret				; Return from subroutine

;***********************************************************
;*	Stored Program Data
;***********************************************************
		; Enter any stored data you might need here

;***********************************************************
;*	Additional Program Includes
;***********************************************************
		; There are no additional file includes for this program