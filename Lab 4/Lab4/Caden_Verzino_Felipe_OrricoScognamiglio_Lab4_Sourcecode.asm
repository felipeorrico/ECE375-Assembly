;************************************************************
;*
;*	Caden_Verzino_Felipe_OrricoScognamiglio_Lab4_Sourcecode.asm
;*
;*	This program implements the ATmega LCD library and prints
;*  two different strings depending on which switches are
;*  pressed. It can also clear the LCD if S8 is pressed.
;*
;*	This is the main code file for Lab4
;*
;************************************************************
;*
;*	 Author: Felipe Orrico Scognamiglio & Caden Verzino
;*	   Date: 01/28/2021
;*
;************************************************************

.include "m128def.inc"			; Include definition file

;************************************************************
;*	Internal Register Definitions and Constants
;************************************************************
.def	mpr = r16				; Multipurpose register is
								; required for LCD Driver

;************************************************************
;*	Start of Code Segment
;************************************************************
.cseg							; Beginning of code segment

;************************************************************
;*	Interrupt Vectors
;************************************************************
.org	$0000					; Beginning of IVs
		rjmp INIT				; Reset interrupt

.org	$0046					; End of Interrupt Vectors

;************************************************************
;*	Program Initialization
;************************************************************
INIT:							; The initialization routine
		; Initialize Stack Pointer
		; Code snippet foud in page 20 of AVR starter guide
		LDI mpr, LOW(RAMEND) ; Low Byte of End SRAM Address
		OUT SPL, mpr ; Write byte to SPL
		LDI mpr, HIGH(RAMEND) ; High Byte of End SRAM Address
		OUT SPH, mpr ; Write byte to SPH

		; Initialize Port D for input (from Lab 1 example code)
		ldi		mpr, $00		; Set Port D Data Direction Register
		out		DDRD, mpr		; for input
		ldi		mpr, $FF		; Initialize Port D Data Register
		out		PORTD, mpr		; so all Port D inputs are Tri-State
		
		; Initialize LCD Display
		RCALL LCDInit			; Initialize the LCD
		RCALL LCDClr			; Clear the LCD
		
		; NOTE that there is no RET or RJMP from INIT, this
		; is because the next instruction executed is the
		; first instruction of the main program

;***********************************************************
;*	Main Program
;***********************************************************
MAIN:							; The Main program
		; Display the strings on the LCD Display
		IN		mpr, PIND		; Get input from Port D
		
		CPI		mpr, $FE        ; Check if S1 is pressed
		BRNE S2					; Branch if not pressed to next CPI
		RCALL Print1			; Call function to print names

		S2: CPI		mpr, $FD	; Check if S2 is pressed
		BRNE S8					; Branch if not pressed to next CPI
		RCALL Print2			; Call function to print names

		S8: CPI		mpr, $7F	; Check if S8 is pressed
		BRNE MAIN				; Branch if not pressed to MAIN
		RCALL LCDClr			; Call function to print names

		rjmp	MAIN			; jump back to main and create an infinite
								; while loop.  Generally, every main program is an
								; infinite while loop, never let the main program
								; just run off

;***********************************************************
;*	Functions and Subroutines
;***********************************************************

;-----------------------------------------------------------
; Func: Print1
; Desc: Prints contents of both strings if S1 is pressed
;-----------------------------------------------------------
Print1: 
		; Selected line 1 for printing
		LDI YL, $00						; Pointing Y to $0100
		LDI YH, $01						; Pointing Y to $0100

		; Load string1 (address) from memory into Z
		LDI ZL, LOW(STRING1_BEG<<1)		; Load String address (LOW) from Program Memory
		LDI ZH, HIGH(STRING1_BEG<<1)	; Load String address (HIGH) from Program Memory 

		Loop:							; Loop for entire string
			CPI ZL, LOW(STRING1_END<<1) ; Check if Z(LOW) is pointing to LOW(STRING1_END<<1)
			BREQ END					; Branch id ZL is equal to ($10 << 1) to END
			
			LPM mpr, Z+					; Load character from Z address into mpr and post-increment Z
			ST Y+, mpr					; Store mpr into (Y) and post-increment Y

			RJMP Loop					; Loop back to Loop
		END:
		RCALL LCDWrLn1					; Print First Line to LCD

		; Selected line 2 for printing
		LDI YL, $10						; Pointing Y to $0110
		LDI YH, $01						; Pointing Y to $0110

		; Load string1 from memory into Z (Address)
		LDI ZL, LOW(STRING2_BEG<<1)		; Load String address (LOW) from Program Memory
		LDI ZH, HIGH(STRING2_BEG<<1)	; Load String address (HIGH) from Program Memory

		Loop2:							; Loop for entire string
			CPI ZL, LOW(STRING2_END<<1) ; Check if Z(LOW) is pointing to LOW(STRING2_END<<1)
			BREQ END2					; Branch if ZL is equal to LOW(STRING2_END<<1)

			LPM mpr, Z+					; Load character from Z address into mpr and post-increment Z
			ST Y+, mpr					; Store mpr into (Y) and post-increment Y

			RJMP Loop2					; Loop to Loop2
		END2:
		RCALL LCDWrLn2					; Print Second Line to LCD

		ret

;-----------------------------------------------------------
; Func: Print2
; Desc: Prints contents of both strings if S2 is pressed
;-----------------------------------------------------------
Print2:
		; Selected line 1 for printing
		LDI YL, $00						; Pointing Y to $0100
		LDI YH, $01						; Pointing Y to $0100

		; Load string1 (address) from memory into Z
		LDI ZL, LOW(STRING2_BEG<<1)		; Load String address (LOW) from Program Memory
		LDI ZH, HIGH(STRING2_BEG<<1)	; Load String address (HIGH) from Program Memory 

		Loop3:							; Loop for entire string
			CPI ZL, LOW(STRING2_END<<1) ; Check if Z(LOW) is pointing to LOW(STRING1_END<<1)
			BREQ END3					; Branch id ZL is equal to ($10 << 1) to END
			
			LPM mpr, Z+					; Load character from Z address into mpr and post-increment Z
			ST Y+, mpr					; Store mpr into (Y) and post-increment Y

			RJMP Loop3					; Loop back to Loop
		END3:
		RCALL LCDWrLn1					; Print First Line to LCD

		; Selected line 2 for printing
		LDI YL, $10						; Pointing Y to $0110
		LDI YH, $01						; Pointing Y to $0110

		; Load string1 from memory into Z (Address)
		LDI ZL, LOW(STRING1_BEG<<1)		; Load String address (LOW) from Program Memory
		LDI ZH, HIGH(STRING1_BEG<<1)	; Load String address (HIGH) from Program Memory

		Loop4:							; Loop for entire string
			CPI ZL, LOW(STRING1_END<<1) ; Check if Z(LOW) is pointing to LOW(STRING2_END<<1)
			BREQ END4					; Branch if ZL is equal to LOW(STRING2_END<<1)

			LPM mpr, Z+					; Load character from Z address into mpr and post-increment Z
			ST Y+, mpr					; Store mpr into (Y) and post-increment Y

			RJMP Loop4					; Loop to Loop2
		END4:
		RCALL LCDWrLn2					; Print Second Line to LCD
		ret

;***********************************************************
;*	Stored Program Data
;***********************************************************

STRING1_BEG:
.DB		"Felipe Orrico   "		; Declaring STRING1 in ProgMem
STRING1_END:
STRING2_BEG:
.DB		"Caden Verzino   "		; Declaring STRING2 in ProgMem
STRING2_END:

;***********************************************************
;*	Additional Program Includes
;***********************************************************
.include "LCDDriver.asm"		; Include the LCD Driver

