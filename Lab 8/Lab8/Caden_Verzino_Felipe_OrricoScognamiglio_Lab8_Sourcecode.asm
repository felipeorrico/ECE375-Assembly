;***********************************************************
;*
;*	Caden_Verzino_Felipe_OrricoScognamiglio_Lab8_Sourcecode.asm
;*
;*	This program will allow the user to transmit morse code
;*	by selecting the desired characters in a menu.
;*	The maximum word length is 16 characters and if the user
;*	tries to confirm more, it will automatically print the 16.
;*
;***********************************************************
;*
;*	 Author: Felipe Orrico Scognamiglio & Caden Verzino
;*	   Date: Mar. 11th, 2021
;*
;***********************************************************

.include "m128def.inc"			; Include definition file

;***********************************************************
;*	Internal Register Definitions and Constants
;***********************************************************
.def	mpr = r16				; Multipurpose register
.def	moder = r8				; Program mode register 
.def	curr_char = r17			; character that is being changed
.def	WH = r19				; store the address of current printed characters
.def	WL = r18
.def	waitcnt = r20			; Wait Loop Counter
.def	ilcnt = r21				; Inner Loop Counter
.def	olcnt = r22				; Outer Loop Counter
.def	letters_confirmed = r24 ; number of confirmed letters

.equ	A_letter = 65			;Letter A ASCII
.equ	Z_letter = 90			;Letter Z ASCII
.equ	WTime = 15				; Time to wait in wait loop

;***********************************************************
;*	Start of Code Segment
;***********************************************************
.cseg							; Beginning of code segment

;***********************************************************
;*	Interrupt Vectors
;***********************************************************
.org	$0000					; Beginning of IVs
		rjmp 	INIT			; Reset interrup
.org	$0046					; End of Interrupt Vectors

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


	; Initialize timer1
		ldi mpr, (1<<CS12) ;256 prescalar
		out TCCR1B, mpr


	; Initialize the LCD Display
		rcall LCDInit ; Initialize LCD Display
		rcall Print_Menu_1
		
		;clear mode and letters register
		clr moder
		clr letters_confirmed

		;loading current character (A)
		ldi curr_char, A_letter 

		;sei	; NOTE: This must be the last thing to do in the INIT function

;***********************************************************
;*	Main Program
;***********************************************************
MAIN:							; The Main program
		IN		mpr, PIND		; Get input from Port D
		
		CPI		mpr, 0b11111110       ; Check if S1 is pressed
		BRNE S6					; Branch if not pressed to next CPI
		RCALL PD0_press			; Call function to print names

		S6: CPI		mpr, 0b11101111	; Check if S2 is pressed
		BRNE S7					; Branch if not pressed to next CPI
		RCALL PD4_press			; Call function to print names

		S7: CPI		mpr, 0b10111111	; Check if S8 is pressed
		BRNE S8					; Branch if not pressed to MAIN
		RCALL PD6_press			; Call function to print names

		S8: CPI		mpr, 0b01111111	; Check if S8 is pressed
		BRNE MAIN				; Branch if not pressed to MAIN
		RCALL PD7_press			; Call function to print names


		rjmp MAIN ; Infinite loop. End of program.


;***********************************************************
;*	Functions and Subroutines
;***********************************************************

;
;	Print_Menu_1
;	Prints initial (as soon as board gets turned on) menu
;
Print_Menu_1:

	push XL
	push XH
	push ZL
	push ZH
	push mpr

	ldi XL, $00						; Pointing X to $0100
	ldi XH, $01						; Pointing X to $0100
	LDI ZL, LOW(MENU_1_LN1_BEG<<1)		; Load String address (LOW) from Program Memory
	LDI ZH, HIGH(MENU_1_LN1_BEG<<1)	; Load String address (HIGH) from Program Memory 

	Loop3:							; Loop for entire string
		CPI ZL, LOW(MENU_1_LN1_END<<1) ; Check if Z(LOW) is pointing to LOW(MENU_1_LN1_END<<1)
		BREQ END					; Branch id ZL is equal to ($10 << 1) to END
			
		LPM mpr, Z+					; Load character from Z address into mpr and post-increment Z
		ST X+, mpr					; Store mpr into (X) and post-increment X

		RJMP Loop3					; Loop back to Loop
	END:
	;RCALL LCDWrLn1					; Print First Line to LCD

	LDI XL, $10						; Pointing Y to $0110
	LDI XH, $01						; Pointing Y to $0110

	LDI ZL, LOW(MENU_1_LN2_BEG<<1)		; Load String address (LOW) from Program Memory
	LDI ZH, HIGH(MENU_1_LN2_BEG<<1)	; Load String address (HIGH) from Program Memory 

	Loop2:							; Loop for entire string
		CPI ZL, LOW(MENU_1_LN2_END<<1) ; Check if Z(LOW) is pointing to LOW(MENU_1_LN2_END<<1)
		BREQ END2					; Branch id ZL is equal to ($10 << 1) to END
			
		LPM mpr, Z+					; Load character from Z address into mpr and post-increment Z
		ST X+, mpr					; Store mpr into (X) and post-increment X

		RJMP Loop2					; Loop back to Loop
	END2:
	RCALL LCDWrite;

	pop mpr
	pop ZH
	pop ZL
	pop XH
	pop XL

	ret

;
;	Print_Menu_2_Ln
;	Prints the second menu (after PD0 is pressed once)
;	This function is only called once, as there is no need to update it every time.
;

Print_Menu_2_Ln1:

	push XL
	push XH
	push ZL
	push ZH
	push mpr
	
	rcall LCDClrLn1					;clear line 1

	ldi XL, $00						; Pointing X to $0100
	ldi XH, $01						; Pointing X to $0100
	LDI ZL, LOW(MENU_2_LN1_BEG<<1)		; Load String address (LOW) from Program Memory
	LDI ZH, HIGH(MENU_2_LN1_BEG<<1)	; Load String address (HIGH) from Program Memory 

	Print_Menu_2_Loop:							; Loop for entire string
		CPI ZL, LOW(MENU_2_LN1_END<<1) ; Check if Z(LOW) is pointing to LOW(MENU_1_LN1_END<<1)
		BREQ Print_Menu_2_END					; Branch id ZL is equal to ($10 << 1) to END
			
		LPM mpr, Z+					; Load character from Z address into mpr and post-increment Z
		ST X+, mpr					; Store mpr into (X) and post-increment X

		RJMP Print_Menu_2_Loop					; Loop back to Loop
	Print_Menu_2_END:
	RCALL LCDWrLn1					; Print First Line to LCD

	rcall LCDClrLn2	

	ldi XL, $10						; Pointing X to $0110
	ldi XH, $01						; Pointing X to $0110

	ldi mpr, A_letter; load to-be-converted value into mpr
	st X, mpr

	RCALL LCDWrLn2

	;W is a register pair that stores the current letter pointer for the 
	;LCD. It allows us to update only that single letter without having to
	;loop and calulate the index.
	mov WL, XL
	mov WH, XH

	pop mpr
	pop ZH
	pop ZL
	pop XH
	pop XL

	ret


;
;	PD0_press
;	This routine has 2 different behaviours. After the boar has just been turned on, the first
;	time that PD0 is pressed, it will enter confirmation mode. After confirmation mode is enabled
;	it will only confirm letters.
;
PD0_press:
	
	push mpr

	;check if the board is not in the initial mode and branch
	ldi mpr, 0
	cp moder, mpr
	brne menu_2_branch_pd0 

	;initial mode
	inc moder
	rcall Print_Menu_2_Ln1
	rcall Wait2

	pop mpr
	ret

	menu_2_branch_pd0: ;confirm mode

	;increment WL so that next letter can be printed
	inc WL

	;load location to store and program memory of each letter
	ldi ZL, low(MORSE_ALPHABET_BEG<<1) 
	ldi ZH, high(MORSE_ALPHABET_BEG<<1)
	ldi XL, low(Confirmed_Letters_BEG)
	ldi XH, high(Confirmed_Letters_BEG)
	
	;here we know the index of the array that the letter is located
	ldi mpr, A_letter
	mov r23, curr_char
	sub r23, mpr ;find the index of the letter in the array in PM
	mov r25, r23
	;loop and increment Z by 2 so that they reflect that index
	Loop_r23:
		cpi r23, $00
		breq end_loop_r23
		dec r23
		adiw Z, 2
		rjmp Loop_r23
	end_loop_r23:
	mov r23, letters_confirmed
	;loop and increment X by 1 so that they reflect that index
	loop_letter_cnt:
		cpi r23, $00
		breq end_loop_cnt
		dec r23
		adiw X, 1
		rjmp loop_letter_cnt
	end_loop_cnt:

	;put letter into array in memory location $0200 + index
	lpm mpr, Z
	st	X, mpr

	;restore r23
	mov r23,r25

	;loading number of symbols in letter from above
	ldi ZL, low(MORSE_ALPHABET_LEN_BEG<<1) 
	ldi ZH, high(MORSE_ALPHABET_LEN_BEG<<1)
	ldi XL, low(Confirmed_Letters_NUM_BEG)
	ldi XH, high(Confirmed_Letters_NUM_BEG)
	
	;same as loops above, but for symbols 
	;per letter instead of the letters themselves
	Loop_r23_2:
		cpi r23, $00
		breq end_loop_r23_2
		dec r23
		;lpm mpr, Z+
		adiw Z, 2
		rjmp Loop_r23_2
	end_loop_r23_2:
	mov r23, letters_confirmed
	loop_letter_cnt_2:
		cpi r23, $00
		breq end_loop_cnt_2
		dec r23
		adiw X, 1
		rjmp loop_letter_cnt_2
	end_loop_cnt_2: 

	;put number of symbols for letter into array in memory location $0210 + index
	lpm mpr, Z
	st	X, mpr

	;"clear" (A) current character
	ldi curr_char, A_letter
	;update the LCD by adding a leading A
	rcall Update_LCD_at_Location
	;increment the number of confirmed letters
	inc letters_confirmed
	;wait for bouncing
	rcall Wait2

	;check if it is on the edge of the LCD
	mov mpr, letters_confirmed
	cpi mpr, $10
	;if it is, "press PD4", else, finish
	brne end_pd0
	rcall PD4_press


	end_pd0: pop mpr
	ret


;
;	PD4_press
;	Routine is executed when switch 5 is pressed.
;	It will print all confirmed characters and clear the arrays afterwards.
;
PD4_press:
	push mpr
	push r23
	
	;check if the program is in confirm mode
	ldi mpr, 0
	cp moder, mpr
	breq PD4_END
	
	;check if letters have been confirmed
	mov mpr, letters_confirmed
	cpi mpr, $00
	breq PD4_END

	;print the letters
	rcall Morse_Print_chars

	;loop to clear memory from letters
	;x will be for letters
	ldi XL, low(Confirmed_Letters_BEG)
	ldi XH, high(Confirmed_Letters_BEG)
	;y will be for num in morse
	ldi YL, low(Confirmed_Letters_NUM_BEG)
	ldi YH, high(Confirmed_Letters_NUM_BEG)
	mov r23, letters_confirmed
	;clear arrays in memory
	PD4_chars_loop_1_beg:
		cpi r23, $00
		breq PD4_chars_loop_1_end
		clr mpr
		st X+, mpr ;the letter to display
		st Y+, mpr ;the number of loops for letter
		dec r23
		rjmp PD4_chars_loop_1_beg
	PD4_chars_loop_1_end:
	;set the amount fo confirmed letters to 0 
	clr letters_confirmed
	;update current letter for A
	ldi curr_char, A_letter
	;update LCD memory location for current letter
	ldi WL, $10	
	ldi WH, $01
	;clear line 2 of lcd
	rcall LCDClrLn2
	;update line 2 of lcd
	rcall Update_LCD_at_Location
	
	PD4_END:
	pop r23
	pop mpr
	ret


;
;	Morse_Print_chars
;	Loops through the arrays in memory and prints each character
;
Morse_Print_chars:
	push mpr
	push r23
	push r25

	clr r25
	clr r23

	;confirmed chars are stored at 0200
	;amount of morse is stored at 0210
	;number of confirmed letter is stored at register letters_confirmed
	;x will be for letters
	ldi XL, low(Confirmed_Letters_BEG)
	ldi XH, high(Confirmed_Letters_BEG)
	;y will be for num in morse
	ldi YL, low(Confirmed_Letters_NUM_BEG)
	ldi YH, high(Confirmed_Letters_NUM_BEG)

	;loop for the amount fo confirmed letters
	mov r23, letters_confirmed
	Morse_Print_chars_loop_1_beg:
		cpi r23, $00
		breq Morse_Print_chars_loop_1_end

		ld mpr, X+ ;the letter to display
		ld r25, Y+ ;the number of loops for letter
		;loop for the amount of symbols in letter
		Letter_loop1_beg:
			cpi r25, $00
			breq Letter_loop1_end
			lsl mpr
			;if carry is set, output dash, else output dot
			brcs Letter_loop1_dash
			rcall DOT
			rjmp Letter_loop1_nxt
			Letter_loop1_dash:
			rcall DASH
			Letter_loop1_nxt:
			;call wait between same letter (1s)
			rcall Wait_clk
			dec r25
			rjmp Letter_loop1_beg
		Letter_loop1_end:
		;wait between other letters (1s (from above) + 2s (below))

		rcall Wait_clk 
		rcall Wait_clk 
		dec r23
		rjmp Morse_Print_chars_loop_1_beg
	Morse_Print_chars_loop_1_end:

	;turn off all LEDs
	ldi mpr, 0b00000000
	out PORTB, mpr

	pop r25
	pop r23
	pop mpr
	ret

;
;	DOT
;	Blinks a DOT
;
DOT:
	push mpr

	;enable dot
	ldi mpr, 0b11110000
	out PORTB, mpr
	rcall Wait_clk
	ldi mpr, 0b00010000
	out PORTB, mpr

	pop mpr
	ret

;
;	DASH
;	Blinks a Dash
;
DASH:
	push mpr

	;enable dot
	ldi mpr, 0b11110000
	out PORTB, mpr
	rcall Wait_clk
	rcall Wait_clk
	rcall Wait_clk
	ldi mpr, 0b00010000
	out PORTB, mpr

	pop mpr

	ret

;
; Update_LCD_at_Location
; Updates the second line of the LCD to display the letter to be selected
;
Update_LCD_at_Location:
	push mpr
	push XH
	push XL

	mov XH, WH
	mov XL, WL
	
	mov mpr, curr_char; load to-be-converted value into mpr
	st X, mpr
	RCALL LCDWrLn2

	pop XL
	pop XH
	pop mpr
	ret


;
; PD6_press
; increases the value in curr_char and checks for overflow to wrap around
;
PD6_press:
	push XL
	push XH
	push ZL
	push ZH
	push mpr

	;check for confirm mode
	ldi mpr, 0
	cp moder, mpr
	breq PD6_END

	;checks if the current character is a Z, and if it is, branch to rotate
	cpi curr_char, Z_letter
	breq PD6_rotate

	;increment character and update LCD
	inc curr_char
	rcall Update_LCD_at_Location
	;wait for bounce
	rcall Wait2

	PD6_END:
	pop mpr
	pop ZH
	pop ZL
	pop XH
	pop XL
	ret

	
	PD6_rotate:
	;change current character to A and update LCD
	ldi curr_char, A_letter
	rcall Update_LCD_at_Location
	;wait for bounce
	rcall Wait2

	pop mpr
	pop ZH
	pop ZL
	pop XH
	pop XL

	ret

;
; PD7_press
; decreases the value in curr_char and checks if it is 0 to wrap around
;
PD7_press:
	
	push XL
	push XH
	push ZL
	push ZH
	push mpr

	;check for program mode
	ldi mpr, 0
	cp moder, mpr
	breq PD7_END

	;check if the letter is an A, and if it is, rotate to Z
	cpi curr_char, A_letter
	breq PD7_rotate

	;decrement current letter and update LCD
	dec curr_char
	rcall Update_LCD_at_Location
	;wait for bounce
	rcall Wait2

	PD7_END:
	pop mpr
	pop ZH
	pop ZL
	pop XH
	pop XL
	ret

	PD7_rotate:

	;set letter to Z and update LCD
	ldi curr_char, Z_letter
	rcall Update_LCD_at_Location
	;wait for bounce
	rcall Wait2

	pop mpr
	pop ZH
	pop ZL
	pop XH
	pop XL

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

;
;	Wait_clk
;	Uses CLK1 to generate a 1s delay
;
Wait_clk:
	push mpr

	;set timer count to 0
	clr mpr
	out TCNT1H, mpr
	out TCNT1L, mpr
	;reset overflow flag
	ldi mpr, 0b00000100
	out TIFR, mpr

	;loop while the flag is not set
	wait_loop:
	in mpr, TIFR
	;check for set flag
	andi mpr, 0b00000100
	breq wait_loop

	;reset the overflow flag
	ldi mpr, 0b00000100
	out TIFR, mpr

	pop mpr
	ret


;***********************************************************
;*	Additional Program Includes
;***********************************************************
.include "LCDDriver.asm"

;***********************************************************
;*	Stored Program Data
;***********************************************************

MENU_1_LN1_BEG:
.DB		"Welcome!        "		; Declaring MENU_1_LN1 in ProgMem
MENU_1_LN1_END:
MENU_1_LN2_BEG:
.DB		"Please press PD0"		; Declaring MENU_1_LN2 in ProgMem
MENU_1_LN2_END:
MENU_2_LN1_BEG:
.DB		"Enter Word:     "		; Declaring MENU_1_LN1 in ProgMem
MENU_2_LN1_END:
;Declaring the morse code for each letter of the alphabet
;The number of symbols for each letter is defined after the letters
;0 - dot, 1 - dash
MORSE_ALPHABET_BEG:
.DB 0b01000000, $00 ;A
.DB 0b10000000, $00 ;B
.DB 0b10100000, $00 ;C
.DB 0b10000000, $00 ;D
.DB 0b00000000, $00 ;E
.DB 0b00100000, $00 ;F
.DB 0b11000000, $00 ;G
.DB 0b00000000, $00 ;H
.DB 0b00000000, $00 ;I
.DB 0b01110000, $00 ;J
.DB 0b10100000, $00 ;K
.DB 0b01000000, $00 ;L
.DB 0b11000000, $00 ;M
.DB 0b10000000, $00 ;N
.DB 0b11100000, $00 ;O
.DB 0b01100000, $00 ;P
.DB 0b11010000, $00 ;Q
.DB 0b01000000, $00 ;R
.DB 0b00000000, $00 ;S
.DB 0b10000000, $00 ;T
.DB 0b00100000, $00 ;U
.DB 0b00010000, $00 ;V
.DB 0b01100000, $00 ;W
.DB 0b10010000, $00 ;X
.DB 0b10110000, $00 ;Y
.DB 0b11000000, $00 ;Z
MORSE_ALPHABED_END:
;Defines number of symbols per letter in morse
MORSE_ALPHABET_LEN_BEG:
.DB $02, $00 ;A
.DB $04, $00 ;B
.DB $04, $00 ;C
.DB $03, $00 ;D
.DB $01, $00 ;E
.DB $04, $00 ;F
.DB $03, $00 ;G
.DB $04, $00 ;H
.DB $02, $00 ;I
.DB $04, $00 ;J
.DB $03, $00 ;K
.DB $04, $00 ;L
.DB $02, $00 ;M
.DB $02, $00 ;N
.DB $03, $00 ;O
.DB $04, $00 ;P
.DB $04, $00 ;Q
.DB $03, $00 ;R
.DB $03, $00 ;S
.DB $01, $00 ;T
.DB $03, $00 ;U
.DB $04, $00 ;V
.DB $03, $00 ;W
.DB $04, $00 ;X
.DB $04, $00 ;Y
.DB $04, $00 ;Z
MORSE_ALPHABET_LEN_END:

;***********************************************************
;*	Data Memory Allocation
;***********************************************************
.dseg

;memory allocation for letters that were confirmed and number of symbols per letter
.org $0200
Confirmed_Letters_BEG: .byte 8
.org $0210
Confirmed_Letters_NUM_BEG: .byte 8