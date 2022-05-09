;Julio Anthony E. Ruiz Coto
;Cesar adrian silva 

;Configuración de Microcontrolador -----------------------------------------------------------
LIST P=16F877A
RADIX HEX
#include <p16F877a.inc>
;---------------------------------------------------------------------------------------------
		
		
;Configuración de Hardware (Configuration Word)-----------------------------------------------
__CONFIG  _CP_OFF &_WDTE_OFF & _PWRTE_OFF & _HS_OSC & _CPD_OFF & _LVP_OFF & _BOREN_OFF & _WRT_OFF & _DEBUG_OFF



CONTADOR1 EQU 0x10
CONTADOR2 EQU 0x20
CONTADOR3 EQU 0x30
;BCF ->(0)
;BSF ->(1)

ORG 0
CLRW ;->limpia el registro W
CLRF PORTA;con esto se limpian los 6 puertos de A
CLRF PORTB;;con esto se limpian los 6 puertos de B
BSF STATUS, RP0 ; BSF = PONER EL BIT EN (1) se va al banco 1 con (1) para activar los puertos
MOVLW 0x06 ;se configuran los 6 puertos como salidas digitales de port A Y B
MOVWF ADCON1 ; para entradas digitales
CLRF TRISA ; limpia TRISA BIT = 0 output --6 bit wide
CLRF TRISB; limpia TRISA BIT = 0 output -- 8 bit wide
BCF STATUS, RP0 ;se selecciona el banco 0 para programar -> RP0(0)



LOOP1  MOVLW H'32' ; MOVE LITERAL VALUE INTO W poner el valor que sigue directamente en el registro W
	   MOVWF CONTADOR1;"Mueve el contenido de W a la dirección de registro que sigue", en este caso, la dirección apunta a contador1

LOOP2  MOVLW H'32'
            MOVWF CONTADOR2

LOOP3  MOVLW H'32'
       MOVWF CONTADOR3

LOOP4 DECFSZ CONTADOR3, f
	  GOTO LOOP4
      ;DECFSZ ->decrementa hasta que no llegue a 0 no salta
      DECFSZ CONTADOR2,f
	  GOTO LOOP3
      DECFSZ CONTADOR1,f
	  GOTO LOOP2
	  CALL LED1
     	  CALL LED2 
      	  GOTO LOOP1




;pregunta si el puerto 5B tiene voltaje
LED1 BTFSC PORTB,5 ;puerto 5B ->salida digital ->pata 38 PIC
         GOTO LED1_ON 
         BSF PORTB,5 ;pone en 1 (pone voltaje en la salida) 5B
         BCF PORTA,0 ;se asegura que el puerto 0A no tenga voltaje
         RETURN
LED1_ON
        ;BCF -> apga el led1 o pone en (0)
        BCF PORTB,5 ;puerto 5B ->salida diigital->pata 38 PIC
        ;BSF->activa el puerto 0A ->(1)
        BSF PORTA,0
        RETURN

;pregunta si el puerto 5B tiene voltaje 
LED2   BTFSC PORTB,5 
       GOTO LED2_ON;->ejecuta siempre y cuando el puerto tenga voltaje (1)
       BSF PORTA,0 ;->coloca (1) al 0A
	   RETURN

LED2_ON
       ;pone el puerto 0A en (0)
       BCF PORTA,0 ;puerto 0A ->pata 2 PIC
	   RETURN

	  END
