/*
	Author: Felipe Orrico Scognamiglio and Caden Verzino
	Date: 01/14/2021
	Version: 1.11.0 FINAL
	Company: Oregon State University
*/

/*
This code will cause a TekBot connected to the AVR board to
move forward and when it touches an obstacle, it will reverse
and turn away from the obstacle and resume forward motion.

PORT MAP
Port B, Pin 4 -> Output -> Right Motor Enable
Port B, Pin 5 -> Output -> Right Motor Direction
Port B, Pin 7 -> Output -> Left Motor Enable
Port B, Pin 6 -> Output -> Left Motor Direction
Port D, Pin 1 -> Input -> Left Whisker
Port D, Pin 0 -> Input -> Right Whisker
*/


#define F_CPU 16000000
#include <avr/io.h>
#include <util/delay.h>
#include <stdio.h>

int main(void)
{
	DDRB = 0b11110000;
	PORTB = 0b11110000;
	DDRD = 0b00000000;
	PORTD = 0b11111111;
	
	PORTB = 0b01100000;     // make TekBot move forward
	
	while (1) // loop forever
	{
		if (PIND == 0b11111100) { //both were hit
			PORTB = 0b00000000;     // move backward
			_delay_ms(1000);         // wait for 1s
			PORTB = 0b00100000;     // turn left
			_delay_ms(1000);        // wait for 1 s
			PORTB = 0b01100000;     // make TekBot move forward
			continue;
		}
		else if (PIND == 0b11111101) {//check for left hit and right not hit
			PORTB = 0b00000000;     // move backward
			_delay_ms(1000);         // wait for 1s
			PORTB = 0b01000000;     // turn right
			_delay_ms(1000);        // wait for 1 s
			PORTB = 0b01100000;     // make TekBot move forward
			continue;
		}
		else if (PIND == 0b11111110) { //check for right hit and left not hit
			PORTB = 0b00000000;     // move backward
			_delay_ms(1000);         // wait for 1s
			PORTB = 0b00100000;     // turn left
			_delay_ms(1000);        // wait for 1 s
			PORTB = 0b01100000;     // make TekBot move forward
			continue;
		}
	}
}

