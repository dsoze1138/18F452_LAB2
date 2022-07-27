;******************************************************************************
;   This file is a basic template for assembly code for a PIC18F452. Copy     *
;   this file into your project directory and modify or add to it as needed.  *
;                                                                             *
;   Refer to the MPASM User's Guide for additional information on the         *
;   features of the assembler.                                                *
;                                                                             *
;   Refer to the PIC18FXX2 Data Sheet for additional information on the       *
;   architecture and instruction set.                                         *
;                                                                             *
;******************************************************************************
;                                                                             *
;    Filename: LAB2 ACTIVITE 1                                                               *
;    Date:       24/07/2022                                                             *
;    File Version:                                                            *
;                                                                             *
;    Author:   ABDULAZIZ HASSOUN                                                                *
;    Company:                                                                 *
;                                                                             * 
;******************************************************************************
;                                                                             *
;    Files Required: P18F452.INC                                              *
;                                                                             *
;******************************************************************************

	LIST P=18F452		;directive to define processor
	#include <P18F452.INC>	;processor specific variable definitions

;******************************************************************************
;Configuration bits
;Microchip has changed the format for defining the configuration bits, please 
;see the .inc file for futher details on notation.  Below are a few examples.



;   Oscillator Selection:
; CONFIG1H
  CONFIG  OSC = HS              ; Oscillator Selection bits (HS oscillator)
  CONFIG  OSCS = OFF            ; Oscillator System Clock Switch Enable bit (Oscillator system clock switch option is disabled (main oscillator is source))

; CONFIG2L
  CONFIG  PWRT = OFF            ; Power-up Timer Enable bit (PWRT disabled)
  CONFIG  BOR = OFF             ; Brown-out Reset Enable bit (Brown-out Reset disabled)
  CONFIG  BORV = 20             ; Brown-out Reset Voltage bits (VBOR set to 2.0V)

; CONFIG2H
  CONFIG  WDT = OFF             ; Watchdog Timer Enable bit (WDT disabled (control is placed on the SWDTEN bit))
  CONFIG  WDTPS = 128           ; Watchdog Timer Postscale Select bits (1:128)

; CONFIG3H
  CONFIG  CCP2MUX = ON          ; CCP2 Mux bit (CCP2 input/output is multiplexed with RC1)

; CONFIG4L
  CONFIG  STVR = ON             ; Stack Full/Underflow Reset Enable bit (Stack Full/Underflow will cause RESET)
  CONFIG  LVP = OFF             ; Low Voltage ICSP Enable bit (Low Voltage ICSP disabled)

; CONFIG5L
  CONFIG  CP0 = OFF             ; Code Protection bit (Block 0 (000200-001FFFh) not code protected)
  CONFIG  CP1 = OFF             ; Code Protection bit (Block 1 (002000-003FFFh) not code protected)
  CONFIG  CP2 = OFF             ; Code Protection bit (Block 2 (004000-005FFFh) not code protected)
  CONFIG  CP3 = OFF             ; Code Protection bit (Block 3 (006000-007FFFh) not code protected)

; CONFIG5H
  CONFIG  CPB = OFF             ; Boot Block Code Protection bit (Boot Block (000000-0001FFh) not code protected)
  CONFIG  CPD = OFF             ; Data EEPROM Code Protection bit (Data EEPROM not code protected)

; CONFIG6L
  CONFIG  WRT0 = OFF            ; Write Protection bit (Block 0 (000200-001FFFh) not write protected)
  CONFIG  WRT1 = OFF            ; Write Protection bit (Block 1 (002000-003FFFh) not write protected)
  CONFIG  WRT2 = OFF            ; Write Protection bit (Block 2 (004000-005FFFh) not write protected)
  CONFIG  WRT3 = OFF            ; Write Protection bit (Block 3 (006000-007FFFh) not write protected)

; CONFIG6H
  CONFIG  WRTC = OFF            ; Configuration Register Write Protection bit (Configuration registers (300000-3000FFh) not write protected)
  CONFIG  WRTB = OFF            ; Boot Block Write Protection bit (Boot Block (000000-0001FFh) not write protected)
  CONFIG  WRTD = OFF            ; Data EEPROM Write Protection bit (Data EEPROM not write protected)

; CONFIG7L
  CONFIG  EBTR0 = OFF           ; Table Read Protection bit (Block 0 (000200-001FFFh) not protected from Table Reads executed in other blocks)
  CONFIG  EBTR1 = OFF           ; Table Read Protection bit (Block 1 (002000-003FFFh) not protected from Table Reads executed in other blocks)
  CONFIG  EBTR2 = OFF           ; Table Read Protection bit (Block 2 (004000-005FFFh) not protected from Table Reads executed in other blocks)
  CONFIG  EBTR3 = OFF           ; Table Read Protection bit (Block 3 (006000-007FFFh) not protected from Table Reads executed in other blocks)

; CONFIG7H
  CONFIG  EBTRB = OFF           ; Boot Block Table Read Protection bit (Boot Block (000000-0001FFh) not protected from Table Reads executed in other blocks)
   

;******************************************************************************
;Variable definitions
; These variables are only needed if low priority interrupts are used. 
; More variables may be needed to store other special function registers used
; in the interrupt routines.

	     CBLOCK	0x080
	    WREG_TEMP	;variable used for context saving 
	    STATUS_TEMP	;variable used for context saving
	    BSR_TEMP	;variable used for context saving
            ENDC

	     CBLOCK	0x000
	    EXAMPLE		;example of a variable in access RAM
            ENDC
LCD_DATA EQU PORTD
LCD_CTRL EQu PORTB
TimerX EQU 0x22 ; Reserving GPR location Ox22 for TimesX
Timerl EQU 0x23 ; Reserving GPR location 0x23 for Timerl
TempLCD EQU 0X24 ; Reserving GPR location 0x24] for TempLCD
RS EQU RB0 ; LCD RS comnacted to port B bit 0
RW EQU RB1 ; LCD RW commected to port B bit 1
EN EQU RB2 ; LCD EN commected to port B bit 2|
               
;******************************************************************************
;EEPROM data
; Data to be programmed into the Data EEPROM is defined here

		ORG	0xf00000

		DE	"Test Data",0,1,2,3,4,5

;******************************************************************************
;Reset vector
; This code will start executing when a reset occurs.

		ORG	0x0000

		goto	Main		;go to start of main code

;******************************************************************************
;High priority interrupt vector
; This code will start executing when a high priority interrupt occurs or
; when any interrupt occurs if interrupt priorities are not enabled.

		ORG	0x0008

		bra	HighInt		;go to high priority interrupt routine

;******************************************************************************
;Low priority interrupt vector and routine
; This code will start executing when a low priority interrupt occurs.
; This code can be removed if low priority interrupts are not used.

		ORG	0x0018

		movff	STATUS,STATUS_TEMP	;save STATUS register
		movff	WREG,WREG_TEMP		;save working register
		movff	BSR,BSR_TEMP		;save BSR register

;	*** low priority interrupt code goes here ***


		movff	BSR_TEMP,BSR		;restore BSR register
		movff	WREG_TEMP,WREG		;restore working register
		movff	STATUS_TEMP,STATUS	;restore STATUS register
		retfie

;******************************************************************************
;High priority interrupt routine
; The high priority interrupt code is placed here to avoid conflicting with
; the low priority interrupt vector.

HighInt:

;	*** high priority interrupt code goes here ***


		retfie	FAST

;******************************************************************************
;Start of main program
; The main program code is placed here.

Main:
;	*** main code goes here ***
 CLRF TRISD
 CLRF TRISB
 BCF LCD_CTRL,EN ; Make LCD_CTRL, EN as low CLRE TRISB

 CALL LDELAY
 MOVLW 0x20 ; Activating line-1 of 4-bit mode LCD
 MOVWF LCD_DATA
 BSF LCD_CTRL,EN ; Make LCD_CTRL, EN as high
 CALL SDELAY
 BCF LCD_CTRL,EN ; Make LCD_CTRL, EN as low
 CALL LDELAY
 MOVLW 0x0E ;LCD Cursor Display on
 CALL COMNWRT
 CALL LDELAY
 MOVLW 0x01 ; Clear LCD Display
 CALL COMNWRT
 CALL LDELAY

 MOVLW 0x06 ;activating entry mode
 CALL COMNWRT
 CALL LDELAY

	    ; Display of letter 'A’

	    MOVLW b'01000000' ; Upper 4 bits of A in binary number system
	    CALL DATAWRT
	    CALL LDELAY
	    MOVLW b'00010000' ; Lower 4 bits of A in binary number system
	    CALL DATAWRT
	    
	    CALL LDELAY ;display er ‘B'

	    MOVLW b'01000010' ; Upper 4 bits of B in binary number system
	    CALL DATAWRT
	    CALL LDELAY
	    MOVLW b'01000010' ; Lower 4 bits of B in binary number system
	    CALL DATAWRT
	    CALL LDELAY

	    ; Display of letter 'D’
            MOVLW b'01000100' ; Upper 4 bits of D in binary number system
	    CALL DATAWRT
	    CALL LDELAY
	    MOVLW b'01000100' ; Lower 4 bits of D in binary number system
	    CALL DATAWRT
	    CALL LDELAY

	    ; Display of letter 'U'

	    MOVLW b'01010101'; Upper 4 bits of I in binary number system
	    CALL DATAWRT
	    CALL LDELAY
	    MOVLW b'01010101' ; Lower 4 bits of I in binary number system
	    CALL DATAWRT
	    CALL LDELAY
	    
	    ;Display of letter 'L’

	    MOVLW b'01000000' ; Upper 4 bits of L in binary number system
	    CALL DATAWRT
	    CALL LDELAY
	    MOVLW b'11000000' ; Lower 4 bits of L in binary number system
	    CALL DATAWRT
	    CALL LDELAY
	    ; Display of letter 'A’

	    MOVLW b'01000000' ; Upper 4 bits of A in binary number system
	    CALL DATAWRT
	    CALL LDELAY
	    MOVLW b'00010000' ; Lower 4 bits of A in binary number system
	    CALL DATAWRT
	    
	    ; Display of letter 'Z’

	    MOVLW b'01011010' ; Upper 4 bits of Z in binary number system
	    CALL DATAWRT
	    CALL LDELAY
	    MOVLW b'01011010' ; Lower 4 bits of Z in binary number system
	    CALL DATAWRT
	    
            ; Display of letter 'I'

	    MOVLW b'01000000' ; Upper 4 bits of I in binary number system
	    CALL DATAWRT
	    CALL LDELAY
	    MOVLW b'10010000' ; Lower 4 bits of I in binary number system
	    CALL DATAWRT
	    CALL LDELAY
	    ; Display of letter 'Z’

	    MOVLW b'01011010' ; Upper 4 bits of Z in binary number system
	    CALL DATAWRT
	    CALL LDELAY
	    MOVLW b'01011010' ; Lower 4 bits of Z in binary number system
	    CALL DATAWRT
	    
	    ; Display of a space between the words

	    MOVLW b'00100000' ; Upper 4 bits of space in binary number system
	    CALL DATAWRT

	    CALL LDELAY

	    MOVLW b'00000000' ; Lower 4 bits of space in binary number system
	    CALL DATAWRT
	    CALL LDELAY
	    
	    ; Display of letter 'H'

	    MOVLW b'01000000' ; Upper 4 bits of H in binary number system
	    CALL DATAWRT
	    CALL LDELAY
	    MOVLW b'10000000' ; Lower 4 bits of H in binary number system
	    CALL DATAWRT
	    CALL LDELAY
	    
	    ; Display of letter 'A’

	    MOVLW b'01000000' ; Upper 4 bits of A in binary number system
	    CALL DATAWRT
	    CALL LDELAY
	    MOVLW b'00010000' ; Lower 4 bits of A in binary number system
	    CALL DATAWRT
	    
	    ; Display of letter 'S’

	    MOVLW b'01010010' ; Upper 4 bits of S in binary number system
	    CALL DATAWRT
	    CALL LDELAY
	    MOVLW b'01010010' ; Lower 4 bits of S in binary number system
	    CALL DATAWRT
	    
	    ; Display of letter 'S’

	    MOVLW b'01010010' ; Upper 4 bits of S in binary number system
	    CALL DATAWRT
	    CALL LDELAY
	    MOVLW b'01010010' ; Lower 4 bits of S in binary number system
	    CALL DATAWRT
	    
	    ; Display of letter 'O'

	    MOVLW b'01001111' ; Upper 4 bits of O in binary number system
	    CALL DATAWRT
	    CALL LDELAY
	    MOVLW b'01001111' ; Lower 4 bits of O in binary number system
	    CALL DATAWRT
	    CALL LDELAY
	    
	    ; Display of letter 'U'

	    MOVLW b'01010101' ; Upper 4 bits of U in binary number system
	    CALL DATAWRT
	    CALL LDELAY
	    MOVLW b'01010101' ; Lower 4 bits of U in binary number system
	    CALL DATAWRT
	    CALL LDELAY
	    ; Display of letter 'N'

	    MOVLW b'01001110' ; Upper 4 bits of N in binary number system
	    CALL DATAWRT
	    CALL LDELAY
	    MOVLW b'01001110' ; Lower 4 bits of N in binary number system
	    CALL DATAWRT
	    CALL LDELAY
AGAIN:
 BTG LCD_CTRL,0
 BRA AGAIN

COMNWRT:
   MOVWF TempLCD
   SWAPF TempLCD,1
   ANDLW 0xF0
   MOVWF LCD_DATA
   BCF LCD_CTRL,RS ; Make LCD_CTRL, RS as low
   BCF LCD_CTRL,RW ; Make LCD_CTRL, RW as low
   BSF LCD_CTRL,EN ; Make LCD_CTRL, EN as high
    CALL SDELAY
    BCF LCD_CTRL,EN ; Make LCD_CTRL, EN as low
    MOVWF TempLCD
    ANDLW 0xF0
    MOVWF LCD_DATA
    BSF LCD_CTRL,EN ; Make LCD_CTRL, EN as high
    CALL SDELAY
    BCF LCD_CTRL,EN ; Make LCD_CTRL, EN as low
    RETURN		
DATAWRT:
	          MOVWF LCD_DATA
		  BSF LCD_CTRL,RS ; Make LCD_CTRL, RS as high
		  BCF LCD_CTRL,RW ; Make LCD_CTRL, RW as low
		  BSF LCD_CTRL,EN ; Make LCD_CTRL, EN as high
		  CALL SDELAY
		  BCF LCD_CTRL,EN ; Make LCD_CTRL, EN as low
		  RETURN

	    ;100 millisecond delay

LDELAY:
	      MOVLW D'100' 
	      MOVWF TimerX ; Count for X ms

LoopX:
		 CALL SDELAY ; Delay lms
		 DECFSZ TimerX ; Repeat X times
		 GOTO LoopX ; until Z
		 RETURN
	
SDELAY:
		MOVLW D'245' ; Count for lms delay
		MOVWF Timerl ; Load count

Loopl:
		NOP ; Pad for 4 cycle loop
		DECFSZ Timerl ; Count
		GOTO Loopl ; until Z
		RETURN
	    
	       
;******************************************************************************
;End of program

		END