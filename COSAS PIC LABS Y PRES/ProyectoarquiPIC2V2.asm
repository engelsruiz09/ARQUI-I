;Julio Anthony E. Ruiz Coto
;Cesar adrian silva 
;Megan Naomi Morales Betancourt 
;Roberto Alfredo Moya Noack

;Configuración de Microcontrolador -----------------------------------------------------------
LIST P=16F877A
RADIX HEX
#include <p16F877a.inc>
;---------------------------------------------------------------------------------------------
		
		
;Configuración de Hardware (Configuration Word)-----------------------------------------------
__CONFIG  _CP_OFF &_WDTE_OFF & _PWRTE_OFF & _HS_OSC & _CPD_OFF & _LVP_OFF & _BOREN_OFF & _WRT_OFF & _DEBUG_OFF


;DECLARACION DE VARIABLES

DUTY_ON     EQU    0X11

;CONTADORES PARA RETARDOS
		CONTADOR1   EQU 0x10
		CONTADOR2   EQU 0x20
		CONTADOR3   EQU 0x30
		CONTADOR4   EQU 0x40
		CONTADOR5   EQU 0X12
		CONTADOR6   EQU 0X13
		
		
		UNIDADES    EQU 0X41
		
		RESULTADO1  EQU 0X50
		RESULTADO2  EQU 0X51
		CONT EQU 0X31
		RELOJ  EQU 0X32 ;SE USA PARA EL PERIODO DEL ADC
		
		DIREC  EQU 0X33
		DATO  EQU 0X34


 ; INICIO DEL PROGRAMA
		ORG       0x00   ; VECTOR DE INICIO
		GOTO      INICIO   ; IR A INICIO
		ORG       0X04     ;AQUI ES DONDE DETECTA SI OCURRE ALGUNA INTERRUPCION EXTERNA
		GOTO      INTERRUP

INICIO
		CLRW                              ;LIMPIA REGISTRO W
		BANKSEL  PORTA                     
		CLRF     PORTA                    ;LIMPIA PORTA 
		CLRF     PORTB                    ;LIMPIA PORTB 
		CLRF     PORTD                    ;LIMPIA PORTD
;======================================================================================
;=====================CONFIGURACION CONVERTIDOR ANALOGICO DIGITAL======================
;======================================================================================

;REGISTRO ADCON1
; |ADF|-|-|-|PCFG3|PCFG2|PCFG1|PCFG0|
		BANKSEL  ADCON1
		MOVLW    B'01001110'  
		MOVWF    ADCON1

;REGISTRO ADCON0
; |ADCS1|ADCS0|CHS2|CHS1|CHS0|GO/DONE|-|ADON|

		BANKSEL  ADCON0
		MOVLW    B'10000001' 
		MOVWF    ADCON0
		CALL     DELAY
;==========================================================================================
;===========================PWM PARA CONTROLAR VELOCIDAD MOTOR DC==========================
;==========================================================================================  
;DATOS OBTENIDOS DE CALCULADOR PIC PWM PARA VARIAR VELOCIDAD MOTOR DC 9VDC
;RELOJ 4MHz
;PWM = 2500 HZ -> ES LA FRECUENCIA PARA MODULAR EL ANCHO DE PULSO(CON ESTO SE VARIA LA POTENCIA HACIA EL MOTOR DE DC)
;RESOLUCION 10 BITS
;PR2 = D'99'
;T2CON = B'00000101'
;CCPR1L = B'00000100'
;CCP1CON= B'00111100'
;AQUI ERA PARA HACER LA SEÑAL DE PULSOS Y REGULAR LA VELOCIDAD DEL MOTOR DE LA BOMBA DE AGUA
		BANKSEL  PR2
		MOVLW    D'99'                   ;SE CARGA PR2  PWM 2500 Hz     
		MOVWF PR2          

		BANKSEL  CCPR1L                  ;SE CARGA CCP1RL PARA PWM SEGUN CALCULADORA 
	    	MOVLW    B'00000100' 
		MOVWF    CCPR1L

		BANKSEL  CCP1CON                 ;SE CARGA CCP1CON  PARA PWM SEGUN CALCULADORA
		MOVLW    B'00111100'
		MOVWF    CCP1CON
		
		BANKSEL  T2CON                   ;SE CARGA T2CON  PARA PWM SEGUN CALCULADORA
		MOVLW    B'00000101'
		MOVWF    T2CON	
			
;==========================================================================================
;===============================CONFIGURACION DE PUERTOS =================================
;==========================================================================================

		BANKSEL TRISA                    ;SELECCION DE BANCO
		CLRF    TRISA                    ;SE LIMPIA REGISTRO 
		MOVLW   B'00000001'              ;SE CONFIGURAN I/O ENTRADAS Y SALIDAS DIGITALES
		MOVWF   TRISB                    ;EN PUERTO B
		CLRF    TRISC                    ;SALIDAS PUERTO C 
		CLRF    TRISD                    ;SALIDAS PUERTO D

		BSF     TRISA, 1                 ;CANAL 0 COMO ENTRADA ANALOGICA
		BSF     TRISA, 0                 ;CANAL 1 COMO ENTRADA ANALOGICA

;==========================================================================================
;============================CONFIGURACION COMUNICACION SERIAL USART=======================
;==========================================================================================

		BCF     TRISC,6                  ;RC6/TX SALIDA, PIN DE TRANSMMISION
		BSF     TRISC,7                  ;RC7/RX ENTRADA, PIN RECEPCION
		MOVLW   D'129'                   ;2400 BAUD RATE XTAL= 4MHz
		MOVWF   SPBRG
		BCF     TXSTA,BRGH               ;SELECCION DE BAJA VELOCIDAD
		BCF     TXSTA,SYNC               ;MODO ASINCRONO

		BCF     STATUS,RP0               ;IR AL BANCO 0
		BCF     STATUS,RP1      
		
		BSF     RCSTA,SPEN               ;HABILITA EL PUERTO SERIAL
		        
		BSF     STATUS,RP0                ;IR AL BANCO 1
		BCF     STATUS,RP1  
		
		BCF     TXSTA,TX9                ; 8 BITS DE DATOS PARA TRANSMISION
		BSF     TXSTA,TXEN               ;ACTIVA LA TRANSMISION SERIAL TXIF=1
		        
		BANKSEL   RCSTA
		BCF     RCSTA,RX9                ;8 BITS DE DATOS
		BSF     RCSTA,CREN               ;PARA RX CONTINUO

		BANKSEL PORTB                    
		CLRF    PORTB                    ;PORTB SALIDA
		CLRF    PORTC                    ;PORTC SALIDA
		CLRF PORTD                    ;PORTD SALIDA

;==========================================================================================
;============================CONFIGURACION TIMER 1 16 BITS=================================
;==========================================================================================

;FORMULA TMR1 = 65536 - [  Xt / ((4/f)*PRE)]

; Xt=  VALOR DE TIEMPO EN ms
; f =  FRECUENCIA OSCILACION EN KHz DEL CRITAL 
; PRE = VALOR DEL ESCALADOR
;TMR1 = VALOR DEL TIMER 1 DE 0 A 65535


;REGISTRO T1CON
; |-|-|T1CKPS1|T1CKPS0|T1OSCEN|T1SYNC|TMRC1CS|TMR1ON|
;VALOR DEL PRE ESCALER 1:8
		BANKSEL  T1CON
		MOVLW    B'00110001'  
		MOVWF    T1CON
  
;================================INTERRUPCION EXTERNA ======================================

;REGISTRO INTCON
;; |GIE|PEIE|TMR0IE|INTE|RBIE|TMR0IF|INTF|RBIF|

;AQUI HABILITAMOS LAS INTERRUPCIONES EXTERNA ESTO SE MIRA CUANDO SE PRECIONA EL PUSHBOTON SOBRE EL RB0

		BANKSEL  INTCON
		BSF      INTCON, PEIE  ;INT Perifericos
		BSF      INTCON, GIE ;INT Globales
		BSF      INTCON, INTE   ;INTERRUPCION EXTERNA RB0/INT
		BCF      INTCON, INTF   ;INTERRUPCION ATENDIDA -> AQUI YA NO HAY INTERRUPCIONES PENDIENTES
		BANKSEL  TMR1L  ;VALOR DECIMAL 3034
		MOVLW    0xDA                      ;
		MOVWF    TMR1L ;EN HEXA 8 BITS MENOS SIGNIFICATIVOS 
		MOVLW    0x0B
		MOVWF    TMR1H ;EN HEXA 8 BITS MAS SIGNIFICATIVOS 

;==========================================================================================
;============================RUTINA PRINCIPAL=================================
;==========================================================================================

PRINCIPAL

		CALL      ADC_1    ;LLAMADA A SUB RUTINA CONVERTIDOR ADC
		BCF       PIR1,RCIF ;SE PONE A 1 EL BIT 5, BUFFER DE RECEPCION ESTA LLENO ->INTERRUPCION QUE SIRVE PARA LA COMUNICACION USART
		BANKSEL   RCREG                   
		MOVFW     RCREG  ; W=RECREG -> ES LA RECEPCION DEL DATO QUE ES ENVIADO POR EL PIC1
		MOVWF     PORTD ;ESCRIBE EN PUERTOD DATO RECIBIDO
        
		BANKSEL   TMR1H
		MOVLW 1
		SUBWF     TMR1H, 0
		BTFSC     STATUS, Z
		CALL      TOGGLE
		GOTO      PRINCIPAL

;==========================================================================================
;============================RUTINA INTERRUPCION EXTERNA=================================
;==========================================================================================
                           ;EXTERNA QUIERE DECIR QUE EL HUMANO PUEDE PARAR EL TRABAJO DEL PIC(ASINCRONA)
 INTERRUP
		CALL      DELAY_20MS               ;LLAMA RETARDO 20MS
		BTFSS     PORTB,0                  ;SI NO ESTA METIDO PUSH
		GOTO      FIN_INT                  ;SALTA A FIN INTERRUP
		BTFSS     PORTD,4                  ;SI ESTA APAGADO ENCIENDE LED
		GOTO      LED_ON                   ;VA A ENCENDER LED 
		BCF       PORTD,4                  ;APAGA LED
		GOTO      FIN_INT
LED_ON
		BSF       PORTD,4                 ;ENCIENDE LED  
FIN_INT
		BCF       INTCON,INTF             ;RESETEA FLAG DE LA INTERRUPCION RB0/INT
		RETFIE    ;FIN DE RUTINA DE INTERRUPCION
TOGGLE 	
		INCF       RELOJ
        		BANKSEL    TMR1L
		MOVLW      0xDA
		MOVWF      TMR1L
		MOVLW      0x0B
		MOVWF      TMR1H
        		RETURN

ADC_1 ;PARA LA BOMBA DE AGUA
		BANKSEL     ADCON0
		BCF ADCON0, CHS0   ;COLOCA CHANNEL 0
		BSF ADCON0, GO;EMPIEZA A CONVERTIR
		BTFSC ADCON0, GO
		GOTO $-1 ;YA TERMINO?

		BANKSEL     ADRESH
		MOVFW ADRESH
		MOVWF RESULTADO1;OBTENER RESULTADO
		MOVWF    DUTY_ON  ;SE GUARDA EL DUTY CICLE 
    
    ;ANCHURA DE PULSO
        
		BANKSEL  CCPR1L
		MOVWF   DUTY_ON  ;SE GUARDA EL DUTY CICLE
		MOVWF  CCPR1L ;SE IGUALA A LA SALIDA CCP1
		CALL DELAY  ;LLAMADA DE RETARDO
		RETURN
;========================RETARDOS============================================ 

DELAY
		CLRF CONT
		MOVLW  0A  ;COLOCA 200 EN EL CONTADOR
		MOVWF CONT
CICLO1
		DECF CONT, 1;DECREMENTA HASTA QUE SEA CERO Y SALE DEL CICLO
		BTFSS STATUS, Z
		GOTO CICLO1
			
		RETURN

DELAY_20MS
;PIC Time Delay = 0.02000000 s with Osc = 4000000 Hz
		movlw	D'26'
		movwf	CONTADOR5
		movlw	D'247'
		movwf	CONTADOR6
loop	decfsz	CONTADOR6,1
		goto	loop
		decfsz	CONTADOR5,1
		goto	loop
		retlw	0

     END
