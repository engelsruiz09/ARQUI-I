		LIST P=16F877A
		RADIX HEX
		#include <p16F877a.inc>
		__CONFIG  _CP_OFF &_WDTE_OFF & _PWRTE_OFF & _HS_OSC & _CPD_OFF & _LVP_OFF & _BOREN_OFF & _WRT_OFF & _DEBUG_OFF
		

	    contador  EQU 0X10	
		;VARIABLES ADC
        RESULTADO1  EQU 0X20
		CONT		EQU 0X30


		ORG 0
		CLRW

		BANKSEL PORTA
		CLRF    PORTA
		CLRF    PORTB
		CLRF 	PORTD

		BANKSEL ADCON1
		MOVLW B'01001110' 
		MOVWF ADCON1

		BANKSEL ADCON0
		MOVLW B'10000001' 
		MOVWF ADCON0
		CALL  DELAY

		BANKSEL TRISA
		CLRF    TRISA
		CLRF    TRISB
		CLRF	TRISD
		BSF     TRISA, 1
		BSF     TRISA, 0

		BANKSEL PORTB
		CLRF    PORTB
		CLRF 	PORTD

		BANKSEL T1CON
		MOVLW B'00110001'  
		MOVWF   T1CON

		BANKSEL TMR1L
		MOVLW  0xDA
		MOVWF  TMR1L
		MOVLW  0x0B
		MOVWF  TMR1H
	

INICIO
		CALL  ADC
		BANKSEL PORTB
		MOVF  RESULTADO1, W
		MOVWF PORTB
		
		BANKSEL TMR1H
		MOVLW 1
		SUBWF TMR1H, 0
		BTFSC STATUS, Z
		CALL  TOGGLE
		GOTO  INICIO

TOGGLE 	
		BANKSEL TMR1L
		MOVLW  0xDA
		MOVWF  TMR1L
		MOVLW  0x0B
		MOVWF  TMR1H
		MOVLW 9
		SUBWF PORTD, 0
		BTFSC STATUS, Z
		GOTO  RESETCOUNTER
		INCF PORTD, 1
		RETURN
RESETCOUNTER
		CLRF PORTD
		RETURN
		

ADC
	BANKSEL ADCON0
	BCF 	ADCON0, CHS0 ;COLOCA CHANNEL 0
	BSF 	ADCON0, GO	 ;EMPIEZA A CONVERTIR
	BTFSC 	ADCON0, GO
	GOTO 	$-1			 ;YA TERMINO?

	BANKSEL ADRESH
	MOVF 	ADRESH,W
	MOVWF 	RESULTADO1	 ;OBTENER RESULTADO
	CALL 	DELAY
	RETURN
	
DELAY
	CLRF 	CONT
	MOVLW 	0A  ;COLOCA 200 EN EL CONTADOR
	MOVWF 	CONT
CICLO1
	DECF 	CONT, 1	;DECREMENTA HASTA QUE SEA CERO Y SALE DEL CICLO
	BTFSS 	STATUS, Z
	GOTO 	CICLO1
	
	RETURN


	END
 