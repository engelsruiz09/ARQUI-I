;UNIVERSIDAD RAFAEL LANDIVAR
;FACULTAD DE INGENIERIA
;ARQUITECTURA DEL COMPUTADOR 1 , CILO 1,2022
;PRELAB 6
;ALUMNO JULIO RUIZ COTO, CESAR SILVA
;Tomando de base el programa y  ejemplo de hoy. 
;Construya un circuito con Displays de 7 segmentos que cuente de  forma decreciente desde 9 hasta 0.
; El periodo del contador será de 500 ms.
; Deberá encender un LED en otro puerto al momento de alcanzar 0.
;Deberá haber un botón que reinicie de nuevo el contador en 9 y apague el LED  de alerta.

;------------CONFIGURACION DEL MICROCONTROLADOR -----------------------------------------------------------
LIST P=16F877A
RADIX HEX
#include <p16F877a.inc>
;---------------------------------------------------------------------------------------------
		
		
;Configuración de Hardware (Configuration Word)-----------------------------------------------
__CONFIG  _CP_OFF &_WDTE_OFF & _PWRTE_OFF & _HS_OSC & _CPD_OFF & _LVP_OFF & _BOREN_OFF & _WRT_OFF & _DEBUG_OFF

CONTADOR1 EQU 0x10
CONTADOR2 EQU 0x20
CONTADOR3 EQU 0x30
CONTADOR4 EQU 0X40
		;BCF ->(0)
		;BSF ->(1)
    ORG 0
    GOTO INICIO
INICIO
               BSF    STATUS,RP0   ;SELECCIONAR BANCO 1. RP0=1
               BCF    STATUS,RP1   ;SELECCIONAR BANCO 1, RP1=0
               MOVLW  0X07         ;EN BINARIO "00000111 "
               MOVWF  TRISA        ;SE CONFIGURA COMO INPUTS LAS PATITAS DEL PIC RA0,RA1,RA2, EL RESTO DE PATITAS QUEDAN COMO OUTPUTS 
               CLRF   TRISB        ;SE PONEN A CERO LAS PATITAS PARA QUE SEAN OUTPUT DIGITALES 
               CLRF   TRISC
               MOVLW  0x06         ;EM BINARIO "00000110" DESHABILITA INPUT ANALOGITAS
               MOVWF  ADCON1       ;TODO EL PUERTO A CONFIGURADO COMO I/O DIGITAL.
               BCF    STATUS,RP0   ;SELECCIONAR BANCO 0, RP0 = 0.
               
;=============================================================
;                   RUTINA QUE LEE TECLA CANAL 0 PUERTO A
;===============================================================
	
LEER_T:   BTFSS   PORTA,0      ;LEE PUERTO RA0 ESPERA QUE SE OPRIMA LA TECLA,VOLTAJE PULL-UP
          GOTO    LOOP         ;SE OPRIME LA TECLA RA0 SALTA A LOOP.
          MOVLW    B'1000000'   ;MIENTRAS NO SE OPRIMA LA TECLA SE MANTIENE EN ESTA LINEA 
          MOVWF    PORTB        ;DISPLAY CON LECTURA EN NUMERO 0
          BSF      TRISC,0      ;ENCIENTE LED
          GOTO     LEER_T       ;REPITE EL CILCO LEER_T

;=============================================================
;                   RUTINA DE CONTEO CON PAUSA
;===============================================================

LOOP
   	 BCF  TRISC,0              ;APAGA LED EN PUERTO C CANAL 0
     CALL PAUSA                ;LLAMA PAUSA
     CALL CONDESC              ;LLAMA SUB RUTINA CONTEO DESCENDENTE
     GOTO LEER_T               ;VUELVE AL CICLO LEER_T

;=============================================================
;                   RUTINA DE CONTEO DESCENDENTE
;===============================================================

CONDESC
    CALL PAUSA 
	MOVLW B'0010000' ;9      GUARDA EN REGISTRO W CONTENIDO DE NUMERO BINARIO 9   
	MOVWF PORTB              ;ESCRIBRE CONTENIDO BINARIO EN REGISTRO W EN PORTB
	CALL PAUSA               ;LLAMA RUTINA DELAY 
 
	MOVLW B'0000000' ;8      GUARDA EN REGISTRO W CONTENIDO DE NUMERO BINARIO 8  
	MOVWF PORTB              ;ESCRIBRE CONTENIDO BINARIO EN REGISTRO W EN PORTB
	CALL PAUSA               ;LLAMA RUTINA DELAY
	
	MOVLW B'1111000' ;7       GUARDA EN REGISTRO W CONTENIDO DE NUMERO BINARIO 7  
	MOVWF PORTB               ;ESCRIBRE CONTENIDO BINARIO EN REGISTRO W EN PORTB
	CALL PAUSA                ;LLAMA RUTINA DELAY

	MOVLW B'0000010' ;6       GUARDA EN REGISTRO W CONTENIDO DE NUMERO BINARIO 6 
	MOVWF PORTB               ;ESCRIBRE CONTENIDO BINARIO EN REGISTRO W EN PORTB
	CALL PAUSA                ;LLAMA RUTINA DELAY

	MOVLW B'0010010' ;5       GUARDA EN REGISTRO W CONTENIDO DE NUMERO BINARIO 5  
	MOVWF PORTB               ;ESCRIBRE CONTENIDO BINARIO EN REGISTRO W EN PORTB
	CALL PAUSA                ;LLAMA RUTINA DELAY

	MOVLW B'0011001' ;4      GUARDA EN REGISTRO W CONTENIDO DE NUMERO BINARIO 4  
	MOVWF PORTB             ;ESCRIBRE CONTENIDO BINARIO EN REGISTRO W EN PORTB
	CALL PAUSA               ;LLAMA RUTINA DELAY

	MOVLW B'0110000' ;3      GUARDA EN REGISTRO W CONTENIDO DE NUMERO BINARIO 3  
	MOVWF PORTB              ;ESCRIBRE CONTENIDO BINARIO EN REGISTRO W EN PORTB
	CALL PAUSA                ;LLAMA RUTINA DELAY

	MOVLW B'0100100' ;2      GUARDA EN REGISTRO W CONTENIDO DE NUMERO BINARIO 2  
	MOVWF PORTB              ;ESCRIBRE CONTENIDO BINARIO EN REGISTRO W EN PORTB
	CALL PAUSA               ;LLAMA RUTINA DELAY

	MOVLW B'1111001' ;1      GUARDA EN REGISTRO W CONTENIDO DE NUMERO BINARIO 1  
	MOVWF PORTB              ;ESCRIBRE CONTENIDO BINARIO EN REGISTRO W EN PORTB
	CALL PAUSA               ;LLAMA RUTINA DELAY
    GOTO LEER_T              ;REGRESA A SUB RUTINA DE LEER TECLA
;=============================================================
;                   RUTINA DE RETARDO (DELAY)
;===============================================================

PAUSA
	;PIC Time Delay = 0.50000200 s with Osc = 4000000 Hz
		movlw	D'3'
		movwf	CONTADOR2
		movlw	D'138'
		movwf	CONTADOR3
		movlw	D'85'
		movwf	CONTADOR4
loop		decfsz	CONTADOR4,1
		goto	loop
		decfsz	CONTADOR3,1
		goto	loop
		decfsz	CONTADOR2,1
		goto	loop
		retlw	0

	END