;UNIVERSIDAD RAFAEL LANDIVAR
;FACULTAD DE INGENIERIA
;ARQUITECTURA DEL COMPUTADOR 1 , CILO 1,2022
;LAB 5
;ALUMNO JULIO RUIZ, CESAR SILVA
;CIRCUITO 1

;Este programa realiza encendido de leds en tres secuencias:
;1.--contador binario ascendente
;2.--cascada ascendente descendente
;3.--parpadeo complementario

LIST P=16F877A
RADIX HEX
#INCLUDE <P16F877a.inc>
__CONFIG  _CP_OFF &_WDTE_OFF & _PWRTE_OFF & _HS_OSC & _CPD_OFF & _LVP_OFF & _BOREN_OFF & _WRT_OFF & _DEBUG_OFF


;;=======================DECLARACION DE VARIABLES=========================================
CONTADOR1  EQU 0x10
CONTADOR2 EQU 0x20
CONTADOR3 EQU 0x30
CONTADOR4 EQU 0X40
CONTADOR6  EQU 0x60 
CONTADOR7  EQU 0x70

 ;-------------------------------- INICIO DE PROGAMA--------------------------------------
        ORG       0x00             ;
        GOTO      INICIO           ; IR A LA ETIQUETA DE INICIO


					 
INICIO
               BSF    STATUS,RP0   ;SELECCIONAR BANCO 1. RP0=1
               BCF    STATUS,RP1   ;SELECCIONAR BANCO 2, RP1=0
               CLRF   PORTA
               MOVLW  0X1F         ;EN BINARIO "00000111 "
               MOVWF  TRISA        ;SE CONFIGURA COMO INPUTS LAS PATITAS DEL PIC RA0,RA1,RA2, EL RESTO DE PATITAS QUEDAN COMO OUTPUTS 
               CLRF   TRISB        ;SE PONEN A CERO LAS PATITAS PARA QUE SEAN OUTPUT DIGITALES 
               MOVLW  0X02         ;
               MOVWF  TRISC        ;SE CONFIGURA PUERTO C NUMERO 2, COMO ENTRADA DIGITAL 
               MOVLW  0XFF         ;
               MOVWF  TRISD;;;     ;SE CONFIGURAN TODOS LOS PUERTOS D ,COMO ENTRADAS DIGITALES
               MOVLW  0x06         ;EM BINARIO "00000110" DESHABILITA INPUT ANALOGITAS
               MOVWF  ADCON1       ;TODO EL PUERTO A CONFIGURADO COMO I/O DIGITAL.
               CLRF   PORTB        ;OCHO PUERTOS B, CONFIGURADO COMO OUTPUT DIGITAL    
               BCF    STATUS,RP0   ;SELECCIONAR BANCO 0, RP0 = 0.
	  
LEER:   BTFSC   PORTA,0               ;VERFICA SI HAY VOLTAJE EN PUERTOA 0,SELECCIONA SECUENCIA DE ENCENDIDO DE LEDS
        GOTO    CONTBIN           ;SI HAY VOLTAJE SALE A RUTINA SELECT_1
        MOVLW    b'00000000'           ;NO HAY VOLTAJE SE QUEDA EN ESPERA CON LOS PUERTOS  
        MOVWF    PORTB                 ;B PUERTOS SIN VOLTAJE
        GOTO     LEER                ;SE QUEDA ESPERANDO ENCONTRAR VOLTAJE EN PORTA,0, 

;=====================================================================================================
;------------------------------SECUENCIA CONTADOR BINARIO------------------------------
;=====================================================================================================   
 

CONTBIN
	    CLRF     CONTADOR7
    	MOVFW    CONTADOR7
    	MOVWF    PORTB

INCREMENTA
	    INCF     CONTADOR7
        MOVFW    CONTADOR7
	    MOVWF    PORTB
        CALL     CAMBIO
        BTFSC    PORTA,0 
        GOTO     CASCADA
        SUBWF    0XFF    ;// ;0XFF - CONTADOR
	    BTFSS    STATUS,Z  ;// DA CERO? NO= INCREMENTA, DA CERO? SI=RE INICIA 
	    GOTO     CONTBIN
	    GOTO     INCREMENTA

;=====================================================================================================
;------------------------------SECUENCIA CASCADA ASCENDENTE DESCENDENTE------------------------------
;=====================================================================================================       

CASCADA: MOVLW   B'10000000'
         MOVWF   PORTB
         CALL   CAMBIO

         MOVLW   B'11000000'
         MOVWF   PORTB
         CALL    CAMBIO

         MOVLW   B'11100000'
         MOVWF   PORTB
         CALL    CAMBIO

         MOVLW   B'11110000'
         MOVWF   PORTB
         CALL    CAMBIO

         MOVLW   B'11111000'
         MOVWF   PORTB
         CALL    CAMBIO

         MOVLW   B'11111100'
         MOVWF   PORTB
         CALL    CAMBIO

         MOVLW   B'11111110'
         MOVWF   PORTB
         CALL    CAMBIO

         MOVLW   B'11111111'
         MOVWF   PORTB
         CALL    CAMBIO

         MOVLW   B'00000000'
         MOVWF   PORTB
         CALL    CAMBIO
;------------------------------------------
         MOVLW   B'00000001'
         MOVWF   PORTB
         CALL    CAMBIO

         MOVLW   B'00000011'
         MOVWF   PORTB
         CALL    CAMBIO

         MOVLW   B'00000111'
         MOVWF   PORTB
         CALL    CAMBIO

         MOVLW   B'00001111'
         MOVWF   PORTB
         CALL    CAMBIO

         MOVLW   B'00011111'
         MOVWF   PORTB
         CALL    CAMBIO

         MOVLW   B'00111111'
         MOVWF   PORTB
         CALL    CAMBIO

         MOVLW   B'01111111'
         MOVWF   PORTB
         CALL    CAMBIO

         MOVLW   B'11111111'
         MOVWF   PORTB
         CALL    CAMBIO

         MOVLW   B'00000000'
         MOVWF   PORTB
         CALL    CAMBIO
         BTFSC   PORTA,0 
         GOTO    PARPADEO
         GOTO    CASCADA
;=====================================================================================================
;------------------------------SECUENCIA PARPADEO COMPLEMENTARIO------------------------------
;=====================================================================================================   
 
PARPADEO:     ;parpadeo
         MOVLW   B'10101010'
         MOVWF   PORTB
         CALL   CAMBIO
         MOVLW   B'01010101'
         MOVWF   PORTB
         CALL    CAMBIO
         BTFSC   PORTA,0 
         GOTO    CONTBIN
         GOTO    PARPADEO

;=====================================================================================================
;------------------------------SECUENCIA DE INCREMENTO Y AUMENTO DE VELOCIDAD--------------------------
;=====================================================================================================   
 


CAMBIO  BTFSC   PORTA,1
        GOTO    TIME_1     ;INCREMENTAR
        BTFSC   PORTA,2
        GOTO    TIME_3    ;DECREMENTR
        CALL    TIME_2
        RETURN
  
;PIC Time Delay = 1.00000200 s with Osc = 4000000 Hz
 TIME_2	movlw	D'6'
		movwf	CONTADOR2
		movlw	D'19'
		movwf	CONTADOR3
		movlw	D'173'
		movwf	CONTADOR4
 loop	decfsz	CONTADOR4,1
		goto	loop
		decfsz	CONTADOR3,1
		goto	loop
		decfsz  CONTADOR2,1
        goto	loop
        retlw	0


TIME_1
;PIC Time Delay = 0.20000000 s with Osc = 4000000 Hz
        movlw    D'2'
        movwf    CONTADOR2
        movlw    D'4'
        movwf    CONTADOR3
        movlw    D'185'
        movwf    CONTADOR4
loop1   decfsz   CONTADOR4,1
        goto     loop1
        decfsz   CONTADOR3,1
        goto     loop1
        decfsz   CONTADOR2,1
        goto     loop1
        retlw    0


       
TIME_3
;PIC Time Delay = 1.50000800 s with Osc = 4000000 Hz
		movlw	D'8'
		movwf	CONTADOR2
		movlw	D'157'
		movwf	CONTADOR3
		movlw	D'7'
		movwf	CONTADOR4
loop3		decfsz	CONTADOR4,1
		goto	loop3
		decfsz	CONTADOR3,1
		goto	loop3
		decfsz	CONTADOR2,1
		goto	loop3
		retlw	0


		END


	 
