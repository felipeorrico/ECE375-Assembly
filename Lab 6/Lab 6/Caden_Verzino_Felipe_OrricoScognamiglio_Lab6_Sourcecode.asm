;***********************************************************
;*
;*	Caden_Verzino_Felipe_OrricoScognamiglio_Lab6_Sourcecode.asm
;*
;*	This code will keep the bot moving forward until it hits
;*	the left or right whisker. That event then triggers an
;*	interrupt, and the necessary code is executed
;*
;***********************************************************
;*
;*	 Author: Felipe Orrico Scognamiglio & Caden Verzino
;*	   Date: Feb. 11th, 2021
;*
;***********************************************************

.include "m128def.inc"			; Include definition file

;***********************************************************
;*	Internal Register Definitions and Constants
;***********************************************************
.def	mpr = r16				; Multipurpose register 

.def	waitcnt = r17			; Wait Loop Counter
.def	ilcnt = r18				; Inner Loop Counter
.def	olcnt = r19				; Outer Loop Counter
.def	lcount = r7			; Counter for left clicks
.def	rcount = r6			; Counter for right clicks
.def	zero = r5

.equ	WTime = 100				; Time to wait in wait loop

.equ	WskrR = 0				; Right Whisker Input Bit
.equ	WskrL = 1				; Left Whisker Input Bit
.equ	EngEnR = 4				; Right Engine Enable Bit
.equ	EngEnL = 7				; Left Engine Enable Bit
.equ	EngDirR = 5				; Right Engine Direction Bit
.equ	EngDirL = 6				; Left Engine Direction Bit

;/////////////////////////////////////////////////////////////
;These macros are the values to make the TekBot Move.
;/////////////////////////////////////////////////////////////

.equ	MovFwd = (1<<EngDirR|1<<EngDirL)	; Move Forward Command
.equ	MovBck = $00				; Move Backward Command
.equ	TurnR = (1<<EngDirL)			; Turn Right Command
.equ	TurnL = (1<<EngDirR)			; Turn Left Command
.equ	Halt = (1<<EngEnR|1<<EngEnL)		; Halt Command

;***********************************************************
;*	Start of Code Segment
;***********************************************************
.cseg							; Beginning of code segment

;***********************************************************
;*	Interrupt Vectors
;***********************************************************
.org	$0000					; Beginning of IVs
		rjmp 	INIT			; Reset interrup

		; Set up interrupt vectors for any interrupts being used
.org	$0002				;INT0 Interrupt Vector
		rcall HitRight		;interrupt handler
		reti

.org	$0004				;INT1 interrupt vector
		rcall HitLeft		;interrupt handler
		reti
			
.org	$0006				;INT2 interrupt vector
		rcall ClearRight	;interrupt handler
		reti

.org	$0008				;INT3 interrupt vector
		rcall ClearLeft		;interrupt handler
		reti

.org	$0046					; End of Interrupt Vectors

;***********************************************************
;*	Additional Program Includes
;***********************************************************
.include "LCDDriver.asm"

;***********************************************************
;*	Program Initialization
;***********************************************************
INIT:							; The initialization routine
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

	; Initialize the LCD Display
		rcall LCDInit ; Initialize LCD Display

		clr zero
		ldi XL, $00						; Pointing Y to $0100
		ldi XH, $01						; Pointing Y to $0100
		mov mpr, zero ; load to-be-converted value into mpr
		rcall Bin2ASCII ; convert value in ASCII
		rcall LCDWrLn1
		LDI XL, $10						; Pointing Y to $0110
		LDI XH, $01						; Pointing Y to $0110
		mov mpr, zero ; load to-be-converted value into mpr
		rcall Bin2ASCII ; convert value in ASCII
		RCALL LCDWrLn2

		; Initialize external interrupts
								; Set the Interrupt Sense Control to falling edge 
		ldi mpr, (1<<ISC01)|(0<<ISC00)|(1<<ISC11)|(0<<ISC10)|(0<<ISC20)|(0<<ISC21)|(0<<ISC30)|(0<<ISC31)
		sts EICRA, mpr			; Use sts, EICRA is in extended I/O space
		; Set the External Interrupt Mask
		ldi mpr, (1<<INT0)|(1<<INT1)|(1<<INT2)|(1<<INT3)
		out EIMSK, mpr
		; Turn on interrupts
		clr rcount
		clr lcount
		sei	; NOTE: This must be the last thing to do in the INIT function

;***********************************************************
;*	Main Program
;***********************************************************
MAIN:							; The Main program
		; Move Robot Forward
		ldi mpr, MovFwd ; Load FWD command
		out PORTB, mpr ; Send to motors
		rjmp MAIN ; Infinite loop. End of program.

;***********************************************************
;*	Functions and Subroutines
;***********************************************************

;----------------------------------------------------------------
; Sub:	ClearRight
; Desc: Clears the counter for right wisker bumps and prints to
;		LCD
;----------------------------------------------------------------
ClearRight:
		rcall LCDClrLn1
		ldi XL, $00						; Pointing Y to $0100
		ldi XH, $01						; Pointing Y to $0100
		mov rcount, zero
		mov mpr, rcount ; load to-be-converted value into mpr
		rcall Bin2ASCII ; convert value in ASCII
		rcall LCDWrLn1

		;clear interrupts
		ldi mpr,0b00001111
		out EIFR, mpr
	ret

;----------------------------------------------------------------
; Sub:	ClearLeft
; Desc: Clears the counter for left wisker bumps and prints to
;		LCD
;----------------------------------------------------------------
ClearLeft:
		rcall LCDClrLn2
		ldi XL, $10						; Pointing Y to $0110
		ldi XH, $01						; Pointing Y to $0110
		mov lcount, zero
		mov mpr, lcount ; load to-be-converted value into mpr
		rcall Bin2ASCII ; convert value in ASCII
		rcall LCDWrLn2

		;clear interrupts
		ldi mpr,0b00001111
		out EIFR, mpr
	ret

;----------------------------------------------------------------
; Sub:	HitRight
; Desc:	Handles functionality of the TekBot when the right whisker
;		is triggered.
;----------------------------------------------------------------
HitRight:
		push	mpr			; Save mpr register
		push	waitcnt			; Save wait register
		in		mpr, SREG	; Save program state
		push	mpr			;
		push	waitcnt			; Wait Loop Counter
		push	ilcnt			; Inner Loop Counter
		push	olcnt			; Outer Loop Counter

		; Move Backwards for a second
		ldi		mpr, MovBck	; Load Move Backward command
		out		PORTB, mpr	; Send command to port
		ldi		waitcnt, WTime	; Wait for 1 second
		rcall	Wait2			; Call wait function

		; Turn left for a second
		ldi		mpr, TurnL	; Load Turn Left Command
		out		PORTB, mpr	; Send command to port
		ldi		waitcnt, WTime	; Wait for 1 second
		rcall	Wait2		; Call wait function

		; Move Forward again	
		ldi		mpr, MovFwd	; Load Move Forward command
		out		PORTB, mpr	; Send command to port

		;add to counter, binary to ascii, print to lcd
		inc rcount

		; load parameters for Bin2ASCII
		ldi XL, $00						; Pointing Y to $0100
		ldi XH, $01						; Pointing Y to $0100
		mov mpr, rcount ; load to-be-converted value into mpr

		rcall Bin2ASCII ; convert value in ASCII

		RCALL LCDWrLn1

		;clear interrupts
		ldi mpr,0b00001111
		out EIFR, mpr

		pop	waitcnt			; Wait Loop Counter
		pop	ilcnt			; Inner Loop Counter
		pop	olcnt			; Outer Loop Counter
		pop		mpr		; Restore program state
		out		SREG, mpr	;
		pop		waitcnt		; Restore wait register
		pop		mpr		; Restore mpr
		ret				; Return from subroutine

;----------------------------------------------------------------
; Sub:	HitLeft
; Desc:	Handles functionality of the TekBot when the left whisker
;		is triggered.
;----------------------------------------------------------------
HitLeft:
		push	mpr			; Save mpr register
		push	waitcnt			; Save wait register
		in		mpr, SREG	; Save program state
		push	mpr			;

		; Move Backwards for a second
		ldi		mpr, MovBck	; Load Move Backward command
		out		PORTB, mpr	; Send command to port
		ldi		waitcnt, WTime	; Wait for 1 second
		rcall	Wait2			; Call wait function

		; Turn right for a second
		ldi		mpr, TurnR	; Load Turn Left Command
		out		PORTB, mpr	; Send command to port
		ldi		waitcnt, WTime	; Wait for 1 second
		rcall	Wait2			; Call wait function

		; Move Forward again	
		ldi		mpr, MovFwd	; Load Move Forward command
		out		PORTB, mpr	; Send command to port

		pop		mpr		; Restore program state
		out		SREG, mpr	;
		pop		waitcnt		; Restore wait register
		pop		mpr		; Restore mpr

		;add to counter, binary to ascii, print to lcd
		inc lcount

		; load parameters for Bin2ASCII
		ldi XL, $10						; Pointing Y to $0100
		ldi XH, $01						; Pointing Y to $0100
		mov mpr, lcount ; load to-be-converted value into mpr

		rcall Bin2ASCII ; convert value in ASCII

		RCALL LCDWrLn2

		;clear interrupts
		ldi mpr,0b00001111
		out EIFR, mpr

		ret				; Return from subroutine

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

