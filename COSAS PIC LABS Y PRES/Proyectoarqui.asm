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
;CONTADORES PARA DELAY
		CONTADOR1 EQU 0x10
		CONTADOR2 EQU 0x20
		CONTADOR3 EQU 0x30
		CONTADOR4 EQU 0x40
		
		CONTADOR5 EQU 0x42
		CONTADOR6 EQU 0x44
		CONTADOR7 EQU 0x46
		CONTADOR8 EQU 0x48
		
		CONTADOR9 EQU 0x52
		CONTADOR10 EQU 0x54
		CONTADOR11 EQU 0x56
		CONTADOR12 EQU 0x58
		CONTADOR13 EQU 0x59
		CONTADOR14 EQU 0x60
		CONTADOR15 EQU 0x61
		CONTADOR16 EQU 0x62
		
		UNIDADES   EQU 0X59
	
		RESULTADO1  EQU 0X50 ;EL QUE LE CARGA EL ADC
		RESULTADO2  EQU 0X51 ;EL QUE LE CARGA EL ADC
		CONT EQU 0X31 
		RELOJ  EQU 0X32 ;DE LA RUTINA DEL ADC


		; INICIO DEL PROGRAMA
		ORG       0x00     ; VECTOR DE INICIO
		GOTO  INICIO  ; IR A INICIO
		;SE CONFIGURAON LOS PUERTOS
		ORG  0x05

INICIO
		CLRW                                    ;LIMPIA REGISTRO W
		BANKSEL PORTA
		CLRF    PORTA
		CLRF    PORTB
		CLRF    PORTC
		CLRF    PORTD

;======================================================================================
;=====================CONFIGURACION CONVERTIDOR ANALOGICO DIGITAL======================
;======================================================================================

;;REGISTRO ADCON1
; |ADF|-|-|-|PCFG3|PCFG2|PCFG1|PCFG0|
;SE USARA PARA LA ENTRADA DEL JOYSTICK ===CONFIGURACION DE PUERTOS ANALOGICOS
		BANKSEL  ADCON1
		MOVLW    B'01001110'  
		MOVWF    ADCON1

;REGISTRO ADCON0
; |ADCS1|ADCS0|CHS2|CHS1|CHS0|GO/DONE|-|ADON|
;A QUE PUERTO SE LE VA A CONECTAR EL POTENCIOMETRO (SEÑAL ANALOGICA)
		BANKSEL  ADCON0
		MOVLW    B'10000001' 
		MOVWF    ADCON0
		CALL     DELAY

;==========================================================================================
;==============================PWM PARA SIRENA ============================================
;==========================================================================================
;DATOS OBTENIDOS DE CALCULADOR PIC PWM PARA VARIAR VELOCIDAD MOTOR DC 9VDC
;RELOJ 4MHz
;PWM = 244 HZ
;RESOLUCION 10 BITS
;PR2 = D'255'
;PREESCALER /16  -> SIRVE PARA ESCALAR EL DATO DEL PWM
;T2CON = B'00000111' ->CONFIGURACION DEL TIMER2
;CCPR1L = B'01011110'
;CCP1CON= B'00001100'  ->ES EL PWM DEL PIC      
		BANKSEL  PR2 ; EL VALOR DEL TIMER2(EL PERIODO DEL REGISTRO) QUE VA A CONTAR
		MOVLW    D'255' 
		MOVWF PR2  ;EL TMR2 ES DE 8 BITS
		
		BANKSEL  CCPR1L       ;SE CARGA CCP1RL PARA PWM SEGUN CALC    
		MOVLW    B'01011110'  
		MOVWF    CCPR1L
		
		BANKSEL  CCP1CON    ;SE CARGA CCP1CON  PARA PWM SEGUN CALCULADORA 
		MOVLW    B'00001100'
		MOVWF    CCP1CON;BIT 3-0 -> 11XX = PWM MODE
		
		BANKSEL  T2CON   ;SE CARGA T2CON  PARA PWM SEGUN CALCULADORA 
		MOVLW    B'00000111'; 1:1 POSTCALE ->BIT 6-3
		MOVWF    T2CON	

;==========================================================================================
;===============================CONFIGURACION DE PUERTOS =================================
;==========================================================================================

		BANKSEL  TRISA   ;SELECCION DE BANCO
		CLRF     TRISA                          ;SE LIMPIA REGISTRO 
		MOVLW    B'00111000'   ;SE CONFIGURAN I/O ENTRADAS Y SALIDAS DIGITALES
		MOVWF    TRISB
		CLRF     TRISC  ;PORTC SALIDAS
		MOVLW    B'1000000'  ;SE CONFIGURAN I/O ENTRADAS Y SALIDAS DIGITALES
		MOVWF    TRISD                          
					
		BSF      TRISA, 1;CANAL 0 COMO ENTRADA ANALOGICA 
		BSF      TRISA, 0 ;CANAL 1 COMO ENTRADA ANALOGICA
		BSF      TRISA, 5  ;ENTRADA DIGITAL PUERTOA, 5

;==========================================================================================
;============================CONFIGURACION COMUNICACION SERIAL USART=======================
;==========================================================================================

		BCF      TRISC,6                ;RC6/TX SALIDA, PIN DE TRANSMMISION
		BSF      TRISC,7              ;RC7/RX ENTRADA, PIN RECEPCION
		MOVLW    D'129'         ;2400 BAUD RATE XTAL= 4MHz
		MOVWF    SPBRG       
		BCF      TXSTA,BRGH                    ;SELECCION DE BAJA VELOCIDAD
		BCF      TXSTA,SYNC                    ;MODO ASINCRONO
		
		BCF      STATUS,RP0                    ;IR AL BANCO 0
		BCF      STATUS,RP1      
		
		BSF      RCSTA,SPEN                    ;HABILITA EL PUERTO SERIAL ->NOMBRE DEL REGISTRO,EL BIT
		        
		BSF      STATUS,RP0                    ;IR AL BANCO 1
		BCF      STATUS,RP1 
		
		BCF      TXSTA,TX9                     ; 8 BITS DE DATOS PARA TRANSMISION
		BSF      TXSTA,TXEN                    ;ACTIVA LA TRANSMISION SERIAL TXIF=1 ->NOMBRE DEL REGISTRO,EL BIT
		         
		BANKSEL   RCSTA
		BCF      RCSTA,RX9                     ; 8 BITS DE DATOS
		BSF      RCSTA,CREN                    ;PARA RX CONTINUO ->RECEPCION

		BANKSEL  PORTB ;SE VA AL BANCO DONDE ESTA EL PORTB -<REGISTRO
		CLRF     PORTB ;SALIDAS
       	                          CLRF     PORTC;SALIDAS DIGITALES
		CLRF  PORTD ;SALIDAS DIGITALES

;==========================================================================================
;============================CONFIGURACION TIMER 1 16 BITS=================================
;==========================================================================================
;FORMULA TMR1 = 65536 - [  Xt / ((4/f)*PRE)]

; Xt=  VALOR DE TIEMPO EN ms ->500ms aprox
; f =  FRECUENCIA OSCILACION EN KHz DEL CRITAL ->4000KHZ
; PRE = VALOR DEL ESCALADOR ->1:8 -(8)
;TMR1 = VALOR DEL TIMER 1 DE 0 A 65535

;REGISTRO T1CON
; |-|-|T1CKPS1|T1CKPS0|T1OSCEN|T1SYNC|TMRC1CS|TMR1ON|
;VALOR DEL PRE ESCALER 1:8

		BANKSEL  T1CON
		MOVLW    B'00110001'  
		MOVWF    T1CON

		BANKSEL  TMR1L                        ;VALOR DECIMAL 3034
		MOVLW    0xDA                       
		MOVWF    TMR1L                       ;EN HEXA 8 BITS MENOS SIGNIFI
		MOVLW    0x0B
		MOVWF    TMR1H                       ;EN HEXA 8 BITS MAS SIGNIFICATI

;================================================================
;============RUTINA PRICIPAL DEL CARRO DE BOMBEROS===============
;================================================================

;SE UTILIZAN LOS GIGUIENTES DIPSWITCH
;DIP 1 (OFF)= HABILITA EL MOVIMIMIENTO DEL CARRO SOLAMENTE
;DIP 1 (ON)= DESHABILITA EL MOTOR DEL CARRO Y HABILITA EL SERVO 1
;DIP 2 (OFF)= DESHABILITA EL SERVO 2
;DIP 2 (ON) = HABILITA EL USO DEL SERVO 2 (MOV. ESCALERA ARRIBA , ABAJO)
;ESTO SE HACE PARA PODER USAR SOLO UN TRIO DE PUSH BOTON PARA MOVER CADA SERVO MOTOR.  
PRINCIPAL
		CALL      ADC_1                      ;LLAMADA SUB RUTINA ADC
		BTFSS     PORTD,7                    ;BOTON PARA HABILITAR SIRENA
		BCF       T2CON,TMR2ON               ;PONEMOS TMR2 EN CERO POR QUE ESTA EL PWM
		BTFSS     PORTD,7                    ;CUANDO NO SE DESEE  
		MOVLW     D'0'                       ;EL USO DE LA SIRENA
		BTFSS     PORTD,7
		MOVWF     CCPR1L
		BTFSS     PORTD,7
		;SE HABILITA EL USO DE LA SIRENA
		BTFSC     PORTD,7                  ;CONDICION PARA HABILITAR SIRENA
		CALL      SIRENA_1                 ;LLAMADA A SUB RUTINA NOTA DO PARA SIRENA
		BTFSC     PORTD,7 
		CALL      DELAY_18MS               ;LLAMADA A RETARDO  18MS
		BTFSC     PORTD,7
		CALL      DELAY_18MS               ;LLAMADA A RETARDO  18MS
		BTFSC     PORTD,7
		CALL      DELAY_1MS                ;LLAMADA A RETARDO  1MS
		BTFSC     PORTD,7
		CALL      DELAY_1MS                ;LLAMADA A RETARDO  1MS
		BTFSC     PORTD,7
		CALL      DELAY_1MS                ;LLAMADA A RETARDO  1MS
		BTFSC     PORTD,7
		CALL      DELAY_1MS                ;LLAMADA A RETARDO  1MS
		BTFSC     PORTD,7
		CALL      SIRENA_2                 ;LLAMADA A SUB RUTINA NOTA FA PARA SIRENA
		CALL      COM_1
		BCF       PIR1,RCIF                ;SE PONE A 1 EL BIT 5, BUFFER 
		BANKSEL   TXREG ;ES EL REGISTRO DONDE SE GUARDA EL DATO QUE SE QUIERE TRANSMITIR  POR USART
		MOVFW     UNIDADES
		MOVWF     TXREG                   ;TXREG =W
		MOVWF     PORTD                   ;ESCRIBE EN PUERTOD DATO RECIBI
		 
		MOVFW     RESULTADO1             ;DATO DEL ADC EN EL REGISTRO ADDRESH
		BTFSC     PORTA,2                ;SE HABILITA EL USO DEL SERVO 1
		GOTO      BRAZO_1                ;LLAMADA DE MOVIMIENTO ROTATIVO ESCALERA CON SERVO 1
		BTFSC     PORTA,5                ; SE HABILITA EL USO DEL SERVO 2 
		GOTO      BRAZO_2                ;LLAMADA DE MOVIMIENTO ARRIBA Y ABAJO DE ESCALERA SERVO 2 
		MOVFW     RESULTADO1             ;LECTURA DEL ADC PARA MOV. DE JOYSTICK HACIA ADELANTE ->ES EL VALOR DIGITAL DEL CONVERTIDOR ->RESULTADO1 = 111111111
		 SUBLW     B'11111111'            ;MASCARA PARA SABER QUE POSICION TIENE JOYSTICK  ->ES LA MAXIMA POSICION DEL JOYSTICK EN VALOR DIGITAL
		BTFSC     STATUS,Z               ;SI LA RESTA ES CERO 
		GOTO      ADELANTE               ;LLAMA SUB RUTINA PARA MOVER EL CARRO HACIA ADELANTE
		MOVFW     RESULTADO1             ;LECTURA DEL ADC PARA MOV. DE JOYSTICK HACIA ATRAS
		ANDLW     B'11111111'            ;MASCARA PAR SABER QUE POSICION TIENE JOYSTICK  -> 0 Y AND DE 1 = 0
		BTFSC     STATUS,Z               ;SI EL RESULTADO ES CERO
		GOTO      ATRAS                  ;MUEVE CARRO HACIA ATRAS 
		        
		BANKSEL   TMR1H
		MOVLW 1
		SUBWF     TMR1H, 0
		BTFSC     STATUS, Z
		CALL      TOGGLE
		GOTO      PRINCIPAL

;CONTADOR DE 0 A 9 , DATO PARA ENVIARLO POR PUERTO SERIAL
COM_1;ES EL COMPARADOR POR QUE SE ESTA USANDO EL REGISTRO STATUS
		INCF     UNIDADES,F             ;INCREMENTA EL VALOR DEL REGISTRO EN 1
		MOVFW    UNIDADES              ;CARGA EL VALOR DEL REGISTRO A W  
		SUBLW    D'10'                 ;RESTA 10 PARA VERIFICAR SI HAY ACARREO     
		BTFSC    STATUS,Z              ;SI ES CERO LA RESTA LA BANDERA Z=1
		CALL     COM_2                 ;COMO ES CERO SE LLEVA EL ACARREO DE 1
		MOVFW    UNIDADES
		 RETURN
COM_2
		CLRF     UNIDADES          ;SE LIMPIA EL REGISTRO QUE LLEVA EL ACARREO
		RETURN 
;================================================================================================
;====================SUB RUTINAS DE MOVIMIENTO ROTATORIO DE ESCALERA CARRO BOMBEROS==============
;================================================================================================
;===============================PWM POR SOFTWARE PARA POSICIONAR SERVO MOTORES ==================
;==========================================SERVO MOTOR 1=========================================

BRAZO_1              
		BTFSC    PORTB,3                   ;SE COMPRUEBA SI EL PIN 3 DEL PUERTO B HA SIDO PRESIONADA
		GOTO     CERO_GRADOS               ;CASO SEA VERDADERO
		BTFSC    PORTB,4                   ;SE COMPRUEBA SI EL PIN 4 DEL PUERTO B HA SIDO PRESIONADA
		GOTO     NOVENTA_GRADOS            ;CASO SEA VERDADERO
		BTFSC    PORTB,5                   ;SE COMPRUEBA SI EL PIN 5 DEL PUERTO B HA SIDO PRESIONADA
		GOTO     MENOS_NOVENTA_GRADOS      ;CASO SEA VERDADERO
		GOTO     PRINCIPAL

;=========================================PWM POR SOFTWARE================================

;TRES MOVIMIENTOS ROTATIVOS,SE MUEVE DE 0, 90 Y 180 GRADOS 
CERO_GRADOS                                 ;POSICION CERO GRADOS
		 ;T = 20 ms  
		;SERVO TRABAJA EN 50HZ ->>1/50 = 20ms
		BSF     PORTB,6                    ;PUERTO SALIDA DUTTY CYCLE SERVO MOTOR 1 EN ON 
		CALL    DELAY_1MS                  ;LLAMDA RETARDO 1MS EN ON = DUTTY CYCLE
		BCF     PORTB,6                    ;PUERTO SALIDA DUTTY CYCLE SERVO MOTOR 1 EN OFF 
		CALL    DELAY_18MS                 ;LLAMDA RETARDO 18 MS EN OFF
		CALL    DELAY_1MS                  ;LLAMDA RETARDO  1 MS EN OFF
		BTFSC   PORTB,3                    ;ANTIRREBOTE
		GOTO    $-1                        ;CASO FALTO SE SIGUE COMPROBANDO
		GOTO    PRINCIPAL                  ;EN CASO VERDADERO SE REGRESA AL PROGRAMA PRINCIPAL
		
NOVENTA_GRADOS	      
		BSF     PORTB,6                    ;PUERTO SALIDA DUTTY CYCLE SERVO MOTOR 1 EN ON 
		CALL    DELAY_1MS                  ;LLAMADA RETARDO 1MS EN ON
		CALL    DELAY_1MS                  ;LLAMADA RETARDO 1MS EN ON
		BCF     PORTB,6                    ;PUERTO SALIDA DUTTY CYCLE SERVO MOTOR 1 EN OFF
		CALL    DELAY_18MS                 ;LLAMADA RETARDO 18 MS EN OFF
		BTFSC   PORTB,4                    ;ANTIRREBOTE
		GOTO    $-1                        ;CASO FALTO SE SIGUE COMPROBANDO
		GOTO    PRINCIPAL                  ;EN CASO VERDADERO SE REGRESA AL PROGRAMA PRINCIPAL

MENOS_NOVENTA_GRADOS
		BSF     PORTB,6                    ;PUERTO SALIDA DUTTY CYCLE SERVO MOTOR 1 EN ON 
		CALL    DELAY_1MS                  ;LLAMADA RETARDO 1MS EN ON
		CALL    DELAY_1MS                  ;LLAMADA RETARDO 1MS EN ON
		CALL    DELAY_0.6MS                ;LLAMADA RETARDO 0.6MS EN ON
		CALL    DELAY_0.1MS                ;LLAMADA RETARDO 0.1MS EN ON
		CALL    DELAY_0.1MS                ;LLAMADA RETARDO 0.1MS EN ON
		BCF     PORTB,6                    ;PUERTO SALIDA DUTTY CYCLE SERVO MOTOR 1 EN OFF
		CALL    DELAY_17MS                 ;LLAMADA RETARDO 17 MS EN OFF 
		CALL    DELAY_0.1MS                ;LLAMADA RETARDO 0.1 MS EN OFF
		CALL    DELAY_0.1MS                ;LLAMADA RETARDO 0.1 MS EN OFF
		BTFSC   PORTB,5                    ;ANTIRREBOTE
		GOTO    $-1                        ;CASO FALTO SE SIGUE COMPROBANDO
		GOTO    PRINCIPAL                  ;EN CASO VERDADERO SE REGRESA AL PROGRAMA PRINCIPAL

;================================================================================================
;====================SUB RUTINAS DE SUBIR Y BAJAR ESCALERA CARRO BOMBEROS==============
;================================================================================================

;==========================================SERVO MOTOR 2=========================================


;SOLO DOS MOVIMIENTOS PARA SUBIR= NOVENTA GRADOS Y BAJAR= CERO GRADOS.
BRAZO_2  
		BTFSC    PORTB,3 ;SE COMPRUEBA SI EL PIN 3 DEL PUERTO B HA SIDO PRESIONADA
		GOTO     CERO_GRADOS_2  ;CASO SEA VERDADERO   
		BTFSC    PORTB,4  ;SE COMPRUEBA SI EL PIN 5 DEL PUERTO B HA SIDO PRESIONADA
		GOTO     NOVENTA_GRADOS_2  ;CASO SEA VERDADERO              
		GOTO     PRINCIPAL

;=========================================PWM POR SOFTWARE================================

CERO_GRADOS_2
        
		BSF     PORTB,7                       ;PUERTO SALIDA DUTTY CYCLE SERVO MOTOR 1 EN ON  
		CALL    DELAY_1MS                     ;LLAMADA RETARDO 1MS EN ON
		BCF     PORTB,7                       ;PUERTO SALIDA DUTTY CYCLE SERVO MOTOR 1 EN OFF  
		CALL    DELAY_18MS                    ;LLAMADA RETARDO 18MS EN OFF
		CALL    DELAY_1MS                     ;LLAMADA RETARDO 1MS EN OFF
		BTFSC   PORTB,3                       ;ANTIRREBOTE 
		GOTO    $-1                           ;CASO FALTO SE SIGUE COMPROBANDO
		GOTO    PRINCIPAL                     ;EN CASO VERDADERO SE REGRESA AL PROGRAMA PRINCIPAL

NOVENTA_GRADOS_2
		BSF     PORTB,7                       ;PUERTO SALIDA DUTTY CYCLE SERVO MOTOR 1 EN ON 
		CALL    DELAY_1MS                     ;LLAMADA RETARDO 1MS EN ON
		CALL    DELAY_1MS                     ;LLAMADA RETARDO 1MS EN ON
		BCF     PORTB,7                       ;PUERTO SALIDA DUTTY CYCLE SERVO MOTOR 1 EN OFF 
		CALL    DELAY_18MS                    ;LLAMADA RETARDO 18MS EN OFF  
		BTFSC   PORTB,4                       ;ANTIRREBOTE
		GOTO    $-1                           ;CASO FALTO SE SIGUE COMPROBANDO
		GOTO    PRINCIPAL                     ;EN CASO VERDADERO SE REGRESA AL PROGRAMA PRINCIPAL

 ;==================================SUBRUTINA RETARDO TIMER1 DEL PIC============================

TOGGLE 	
		INCF      RELOJ
		BANKSEL   TMR1L
		MOVLW     0xDA
		MOVWF     TMR1L
		MOVLW     0x0B
		MOVWF     TMR1H
		RETURN

;======================================================================================
;====================SUB RUTINA CONVERTIDOR ANALOGICO DITIAL=============================
;=======================================================================================

ADC_1             ;AQUI ES DONDE SE GUARDA EL DATO DEL CONVERTIDOR QUE VIENE DEL REGISTRO ADRESH
		BANKSEL     ADCON0
		BCF ADCON0, CHS0               ;COLOCA CHANNEL 0
		BSF ADCON0, GO ;EMPIEZA A CONVERTIR
		BTFSC  ADCON0, GO
		GOTO        $-1;YA TERMINO?
		
		BANKSEL     ADRESH
		MOVF ADRESH,W
		MOVWF RESULTADO1;OBTENER RESULTADO
		    
		CALL DELAY
		RETURN
      
;======================================================================================
;====================SUB RUTINA MOVIMIENTO DE CARRO MOTOR POR JOYSTIC=============================
;=======================================================================================

;JOYSTICK POTENCIOMETRO (X) AL VALOR MAX, ADC = 5 VOLT, HACE QUE ENTRE EN ESTA CONDICION
ADELANTE
		MOVLW       B'00000101'  ;HACIA ADELANTE IN =1, IN2=0 Y EN1=1
		MOVWF       PORTB                    ;SE CARGA EL VALOR DE W A PORTB
		CALL        SECUENCIA_RED_BLUE
		CALL        PARO                     ;LLAMA SUBRUTINA PARO
		GOTO        PRINCIPAL                ;VUELVE AL CICLO
     

;JOYSTICK POTENCIOMETRO (X) AL VALOR MIN, ADC = 0 VOLT, HACE QUE ENTRE EN ESTA CONDICION
ATRAS
		MOVLW         B'00000110' ;HACIA ATRAS IN =0, IN2=1 Y EN1=1 
		MOVWF         PORTB   ;SE CARGA EL REGISTRO PORTB CON EL VALOR CARGADO EN W
		CALL          RETARDO ;PARA QUE TENGA UN TIEMPO ENTRE MOVER HACIA ATRAS Y ENCENDER LAS LUCES
		CALL          SECUENCIA_RB
		CALL          PARO
		GOTO          PRINCIPAL  ;VUELVE AL CICLO
   
;JOYSTICK POTENCIOMETRO (X) AL VALOR MEDIO, ADC = 2.5 VOLT, HACE QUE ENTRE EN ESTA CONDICION
PARO
		MOVLW        B'00000000' ;MOTOR DC EN PARO EN1=0
		MOVWF        PORTB ;SE CARGA EN EL REGISTRO PORTB EL VALOR CARGADO EN W
		GOTO         PRINCIPAL

;========================================================================
;=======================SUB RUTINA SIRENA CAMION DE BOMBEROS=============
;==================================TIMER 2 PWM =========================== 
;========================================================================

;=======================NOTA MUSICAL DO =================================
;PARA EMULAR LA SIRENA DE UN CAMION DE BOMBEROS USAMOS NOTAS MUSICALES

;NOTA DO PARA QUE SEA AUDIBLE POR LA BOCINA
;CRISTAL DE 4MHz
;=======================================
;PARA GENERAR OSCILACION CON UN PERIODO T

; PR2 = [  T / (PRE*4/Fosc)] - 1

; T   = PERIODO EN MILISEGUNDOS
; PRE = VALOR DEL PREESCALER
; PR2 = REGISTRO PARA EL PERIDO
; Fosc = FRECUENCIA DEL CRISTAL EN KHZ

;PARA GENERAR EL DUTTY CICLE

;CCPR1L = X*% PR2

;CCPR1L  = REGISTRO PARA EL DUTTY CICLE
;PR2     = VALOR CALCULADO PAR EL PERIODO
;X       = PORCENTAJE DEL PERIODO (DUTTY) ->50%


;NOTA DO = 261.63 Hz ---> 3.82 MS

; PR2 = [3.82/(16*4/4000)]-1

; PR2 = 238 ->ES EL EQUIVALENTE DE LAS UNIDADES DE TIEMPO DE LA NOTA DO

;50% DUTTY

;CCPR1L = 238*0.5 = 119.

SIRENA_1
		BSF     T2CON,TMR2ON ;SE ENCIENDE EL TMR2
		BSF     STATUS,RP0
		BCF     STATUS,RP1  ;VAMOS AL BANCO 1  
		MOVLW   D'238' ;SE CARGA EL VALOR DE LA FRECUENCIA DE LA NOTA DO ->EL PERIODO
		MOVWF   PR2   ;EN EL REGISTRO PR2
		BCF     STATUS,RP0
		BCF     STATUS,RP1   ;VAMOS AL BANCO 0 
		MOVLW   D'119'   ;SE CARGA EL VALOR DEL DUTY CICLE 50% DEL PR2->DUTTY CYCLE
		MOVWF   CCPR1L  ;EN REGISTRO CCPR1L
		RETURN   ;EN CASO VERDADERO SE REGRESA AL PROGRAMA PRINCIPAL

;=====================NOTA MUSICAL FA==================================

;NOTA FA = 349.23 Hz ---> 2.86 MS

; PR2 = [2.86/(16*4/4000)]-1

; PR2 = 177

;50% DUTTY

;CCPR1L = 177*0.5 = 89.

 SIRENA_2
		BSF     T2CON,TMR2ON         ;SE ENCIENDE EL TMR2
		BSF     STATUS,RP0
		BCF     STATUS,RP1           ;VAMOS AL BANCO 1  
		MOVLW   D'177'               ;SE CARGA EL VALOR DE LA FRECUENCIA DE LA NOTA FA->EL PERIODO
		MOVWF   PR2                  ;EN EL REGISTRO PR2
		BCF     STATUS,RP0
		BCF     STATUS,RP1           ;VAMOS AL BANCO 0 
		MOVLW   D'89'                ;SE CARGA EL VALOR DEL DUTY CICLE 50% DEL PR2->DUTTY CYCLE
		MOVWF   CCPR1L               ;EN REGISTRO CCPR1L    
		RETURN

;================SUB RUTINA ENCENDIDO Y APAGADO DE LUCES CAMION BOMBEROS ============
     
SECUENCIA_RED_BLUE
		MOVLW   B'00010000'          ;ESCRIBE SOBRE SALIDAS PORTC ENCIENDE LUCES
		MOVWF   PORTC
		CALL    CAMBIO               ;LLAMADA DE RETARDO
		MOVLW   B'00011000'          ;ESCRIBRE SOBRE SALIDAS PORTC ENCIENTE LUCES 
		MOVWF   PORTC
		CALL    CAMBIO               ;LLAMADA DE RETARDO
		RETURN

SECUENCIA_RB
		MOVLW  B'00000000'           ;ESCRIBE SOBRE SALIDAS PORTC APAGA LUCES
		MOVWF  PORTC
		CALL   CAMBIO                ;LLAMADA DE RETARDO
		MOVLW  B'00011000'            ;ESCRIBRE SOBRE SALIDAS PORTC ENCIENTE LUCES 
		MOVWF  PORTC
		CALL   CAMBIO                ;LLAMADA DE RETARDO
		 RETURN
           
;=============================================================================
;===========================SUB RUTINAS DE RETARDOS DE TIEMPO================
;=============================================================================

;RETARDOS UTILIZADOS EN MOVIMIENTO PARA: ADC, SERVO MOTORES, SIRENA Y LUCES INTERMI
;TENTES.

;;;;;;;RETARDO;;;;
RETARDO
		MOVLW H'0A'
		MOVWF CONTADOR1

		LOOP1  
		MOVLW H'64'
		MOVWF CONTADOR2
		
		LOOP2  
		MOVLW H'C8'
		MOVWF CONTADOR3
		
		LOOP3  
		DECFSZ CONTADOR3, f
		GOTO LOOP3
		DECFSZ CONTADOR2,f
		GOTO LOOP2
		DECFSZ CONTADOR1,f
		GOTO LOOP1
		RETURN

DELAY_0.1MS
;PIC Time Delay = 0.00009500 s with Osc = 4000000 Hz
		movlw	D'30'
		movwf	CONTADOR14
loop_6		decfsz	CONTADOR14,1
		goto	loop_6
		retlw	0

DELAY_0.2MS
;PIC Time Delay = 0.00020000 s with Osc = 4000000 Hz
		movlw	D'65'
		movwf	CONTADOR13
loop_5		decfsz	CONTADOR13,1
		goto	loop_5
		retlw	0



DELAY_0.6MS
;PIC Time Delay = 0.00059900 s with Osc = 4000000 Hz
		movlw	D'198'
		movwf	CONTADOR4
loop_4		decfsz	CONTADOR4,1
		goto	loop_4
		retlw	0

DELAY_1MS

;PIC Time Delay = 0.00100100 s with Osc = 4000000 Hz
		movlw	D'2'
		movwf	CONTADOR5
		movlw	D'74'
		movwf	CONTADOR6
loop_1		decfsz	CONTADOR6,1
		goto	loop_1
		decfsz	CONTADOR5,1
		goto	loop_1
		retlw	0

DELAY_17MS
;PIC Time Delay = 0.01700000 s with Osc = 4000000 Hz
		movlw	D'23'
		movwf	CONTADOR15
		movlw	D'17'
		movwf	CONTADOR16
loop		decfsz	CONTADOR16,1
		goto	loop
		decfsz	CONTADOR15,1
		goto	loop
		retlw	0



DELAY_18MS
;PIC Time Delay = 0.01800100 s with Osc = 4000000 Hz
		movlw	D'24'
		movwf	CONTADOR7
		movlw	D'94'
		movwf	CONTADOR8
loop_2		decfsz	CONTADOR8,1
		goto	loop_2
		decfsz	CONTADOR7,1
		goto	loop_2
		retlw	0



CAMBIO	
;PIC Time Delay = 0.05000300 s with Osc = 4000000 Hz
		movlw	D'65'
		movwf	CONTADOR11
		movlw	D'238'
		movwf	CONTADOR12
loop_3		decfsz	CONTADOR12,1
		goto	loop_3
		decfsz	CONTADOR11,1
		goto	loop
		retlw	0

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

