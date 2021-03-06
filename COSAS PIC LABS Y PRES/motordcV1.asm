
;UNIVERSIDAD RAFAEL LANDIVAR
;FACULTAD DE INGENIERIA
;ARQUITECTURA DEL COMPUTADOR 1 , CILO 1,2022
;LAB 5
;ALUMNO JULIO RUIZ, CESAR SILVA
;CIRCUITO 2
;CONTROL DIRECCION DE GIRO DE MOTOR DC USANDO:
;PIC16F877A.
;PUENTE H (L293D).
;TRES BOTONES: BOTON 1 = GIRO HACIA ADELANTE
;BOTON 2 = DETENER, BOTON 3, GIRO HACIA ATRAS.
;SE PRETENDE:
;PRESIONANDO EL BOTON CONECTADO EN LA PATITA (RA0), COMMIENZA A GIRAR EL MOTOR HACIA ADELANTE
;PRESIONANDO EL BOTON CONECTADO EN LA PATITA (RA1), EL MOTOR DE DC GIRA PARA ATRAS. 
;PARA DETENER EL MOTOR EL BOTON SE CONECTA EN LA PATITA (RA2)
;SE CONECTARA EL MOTOR A UN PUENTE A TRAVES DEL PUERTO C
;------------CONFIGURACION DEL MICROCONTROLADOR -----------------------------------------------------------
LIST P=16F877A
RADIX HEX
#include <p16F877a.inc>
;---------------------------------------------------------------------------------------------
		
		
;Configuración de Hardware (Configuration Word)-----------------------------------------------
__CONFIG  _CP_OFF &_WDTE_OFF & _PWRTE_OFF & _HS_OSC & _CPD_OFF & _LVP_OFF & _BOREN_OFF & _WRT_OFF & _DEBUG_OFF


;;CONTADORES PARA DELAY


CONTADOR1 EQU 0x10
CONTADOR2 EQU 0x20
CONTADOR3 EQU 0x30



; Inicio del programa
        ORG       0x00 ; Vector de Inicio
        GOTO      INICIO ; Ir a la etiqueta 'Inicio'
;SE CONFIGURAON LOS PUERTOS
       ORG  0x05
INICIO
     BSF    STATUS,RP0   ;SELECCIONAR BANCO 1. RP0=1
     BCF    STATUS,RP1   ;SELECCIONAR BANCO 2, RP1=0
     MOVLW  0X07         ;EN BINARIO "00000111 "
     MOVWF  TRISA        ;SE CONFIGURA COMO INPUTS LAS PATITAS DEL PIC RA0,RA1,RA2, EL RESTO DE PATITAS QUEDAN COMO OUTPUTS 
     CLRF   TRISB        ;SE PONEN A CERO LAS PATITAS PARA QUE SEAN OUTPUT DIGITALES 
     MOVLW  0x06         ;EM BINARIO "00000110" DESHABILITA INPUT ANALOGITAS
     MOVWF  ADCON1       ;TODO EL PUERTO A CONFIGURADO COMO I/O DIGITAL.
     BCF    STATUS,RP0   ;SELECCIONAR BANCO 0, RP0 = 0.
     CLRF   PORTB        ;PUERTO B CONFIGURADO COMO OUTPUT DIGITAL    

;CONFIGURACION DETECCION BOTONES PULSADORES NORMALMENTE ABIERTOS;;;;;;;;;;;;;

LOOP_0                  ;LOS BOTONES ESTAN CONECTADOS PULL DOWN (ESTAN CON VOLTAJE)
     BTFSC PORTA,0       ;SI EL BIT RA0 DEL REGISTRO PORTA  PULL-DOWN TA =0, BRINCA LA SIGU. INTRUCCION
     GOTO  ADELANTE      ;SENTIDO HORARIO
     BTFSC PORTA,1       ;SI EL BIT RA1 =1, SALTA LA SIGUIENTE INTRUCCION
     GOTO  ATRAS         ;SENTIDO ANTIHORARIO
     GOTO  LOOP_0       ;VUELVE AL CICLO

ADELANTE
     MOVLW b'00000101'  ;HACIA ADELANTE IN =1, IN2=0 Y EN1=1
     MOVWF PORTB         ;SE CARGA EL VALOR DE W A PORTB
     CALL  DELAY         ;LLAMA SUBRUTINA DELAY
     CALL  PARO          ;LLAMA SUBRUTINA PARO
     CALL  LOOP_1       ;LLAMA SUBRUTINA BOTON_1
     GOTO  ADELANTE      ;VUELVE AL CICLO

ATRAS
     MOVLW b'00000110'   ;HACIA ATRAS IN =0, IN2=1 Y EN1=1 
     MOVWF PORTB         ;SE CARGA EL REGISTRO PORTB CON EL VALOR CARGADO EN W
     CALL  DELAY
     CALL  PARO
     CALL  LOOP_2
     GOTO  ATRAS          ;VUELVE AL CICLO


LOOP_1
     BTFSC  PORTA,1      ;SI EL BIT RA1 DEL REGISTRO PORTA ES 0 BRINCA LA SIGUIENTE INSTRUCCION
     GOTO   ATRAS        ; VA A LA SUBRUTINA GIRO ANTIRELOJ LLAMADA ATRAS
     GOTO   TO_BACK

LOOP_2
     BTFSC  PORTA,0      ;SI EL BIT RA1 DEL REGISTRO PORTA ES 0 BRINCA LA SIGUIENTE INSTRUCCION
     GOTO   ADELANTE     ; VA A LA SUBRUTINA GIRO HORARIO LLAMADA ADELANTE
     GOTO   TO_BACK

PARO
     BTFSS  PORTA,2       ;SI EL BIT RA2 DEL REGISTRO PORTA ES 1 BRINCA LA SIG. INSTRUCCION
     GOTO   TO_BACK
     MOVLW  b'00000000'   ;MOTOR DC EN PARO EN1=0
     MOVWF  PORTB          ;SE CARGA EN EL REGISTRO PORTB EL VALOR CARGADO EN W
     GOTO   LOOP_0

TO_BACK
     RETURN
     


;;;;;;;RETARDO;;;;
DELAY
       MOVLW H'0A'
	  MOVWF CONTADOR1

LOOP1  MOVLW H'64'
	  MOVWF CONTADOR2

LOOP2  MOVLW H'C8'
       MOVWF CONTADOR3

LOOP3 	  DECFSZ CONTADOR3, f
	  GOTO LOOP3
	  DECFSZ CONTADOR2,f
	  GOTO LOOP2
	  DECFSZ CONTADOR1,f
	  GOTO LOOP1
	  

    RETURN

     END
  
     
