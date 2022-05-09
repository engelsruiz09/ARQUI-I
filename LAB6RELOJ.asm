;UNIVERSIDAD RAFAEL LANDIVAR
;FACULTAD DE INGENIERIA
;ARQUITECTURA DEL COMPUTADOR 1 , CICLO 1,2022
;LAB 6
;ALUMNO JULIO RUIZ, CESAR SILVA
;CIRCUITO RELOJ DESPERTADOR

;FUNCIONALIDAD
;Los Display despliegan el valor de la hora indicada, y la función de los botones es descrita 
;con detalle a continuación:
;1. Aumentar Minutos: Al presionar este botón se aumenta el contador de 
;minutos del reloj o de la alarma, según sea el modo que se encuentre activo.
;2. Aumentar Horas: Al ser presionado se aumenta el contador de las horas del 
;reloj o de la alarma, según sea el modo activo.
;3. Set Alarma: Se utiliza para intercambiar el dispositivo entre el modo de reloj
;y el modo de alarma, usado para colocar esta. 
;4. Snooze: Cuando la alarma esta activada, el snooze la apaga retrasándola 10 
;minutos hasta que vuelve a sonar nuevamente.
;5. Detener Alarma: Detiene el sonar de la alarma cuando esta es activada, 
;basta únicamente con volver a colocar la alarma para que vuelva a funcionar 
;normalmente.
;------------CONFIGURACION DEL MICROCONTROLADOR -----------------------------------------------------------
LIST P=16F877A
RADIX HEX
#include <p16F877a.inc>
;---------------------------------------------------------------------------------------------
 
;Configuración de Hardware (Configuration Word)-----------------------------------------------
__CONFIG  _CP_OFF &_WDTE_OFF & _PWRTE_OFF & _HS_OSC & _CPD_OFF & _LVP_OFF & _BOREN_OFF & _WRT_OFF & _DEBUG_OFF
		
;===================================DECLARACION DE VARIABLES AUTILIZAR================
SEG_UNIDADES         EQU   H'50'            ;REGISTRO PARA LAS UNIDADES DE SEGUNDOS
SEG_DECENAS          EQU   H'51'            ;REGISTRO DE LAS DECENAS DE SEGUNDOS
MIN_UNIDADES         EQU   H'52'            ;REGISTRO PARA LAS UNIDADES DE MINUTOS
MIN_DECENAS          EQU   H'53'            ;REGISTRO DE LAS DECENAS DE MINUTOS
HORA_UNIDADES        EQU   H'54'            ;REGISTRO PARA LAS UNIDADES DE MINUTOS
HORA_DECENAS         EQU   H'55'            ;REGISTRO DE LAS DECENAS DE MINUTOS
 
ADDR_L               EQU   H'56'            ;DIRECCION PARA GUARDAR EN MEMORIA EEPROM
DATA_L               EQU   H'57'            ;DATO A GUARDAR


SEG_UNIDADES_AL      EQU   H'60'            ;REGISTRO PARA LAS UNIDADES DE SEGUNDOS
SEG_DECENAS_AL       EQU   H'61'            ;REGISTRO DE LAS DECENAS DE SEGUNDOS
MIN_UNIDADES_AL      EQU   H'62'            ;REGISTRO PARA LAS UNIDADES DE MINUTOS
MIN_DECENAS_AL       EQU   H'653'            ;REGISTRO DE LAS DECENAS DE MINUTOS
HORA_UNIDADES_AL     EQU   H'64'            ;REGISTRO PARA LAS UNIDADES DE MINUTOS
HORA_DECENAS_AL      EQU   H'65'            ;REGISTRO DE LA


;P1                   EQU    H'66'
;P2                   EQU    H'67' 


CONTADOR1  EQU 0x10
CONTADOR2 EQU 0x20
CONTADOR3 EQU 0x30
CONTADOR4 EQU 0X40

CONTADOR5  EQU 0x42
CONTADOR6 EQU 0x44
CONTADOR7 EQU 0x46
CONTADOR8 EQU 0X48

 ORG    0x2100        ; Esta es una parte que no he entendido bien
    DE    H'0'        ; Pero la utilize con el pic 16f628
    DE    H'0'        ; y funciona, Ahora bien Si no coloco esto
    DE    H'0'        ; NO FUNCIONARA.
    DE    H'0' 

		;BCF ->(0)
		;BSF ->(1)
    ORG 0
    GOTO INICIO
INICIO
               BCF    STATUS,RP0   ;SELECCIONAR BANCO 1. RP0=1
               BCF    STATUS,RP1   ;SELECCIONAR BANCO 2, RP1=0
               CLRF   PORTA
               CLRF   PORTC
               BSF    STATUS,RP0   ;SELECCIONAR BANCO 1. RP0=1
               BCF    STATUS,RP1   ;SELECCIONAR BANCO 2, RP1=0
               MOVLW  0X3F         ;EN BINARIO "00111111 "
               MOVWF  TRISA        ;SE CONFIGURA COMO INPUTS LAS PATITAS DEL PIC RA0,RA1,RA2, EL RESTO DE PATITAS QUEDAN COMO OUTPUTS 
               CLRF   TRISB        ;SE PONEN A CERO LAS PATITAS PARA QUE SEAN OUTPUT DIGITALES 
               CLRF   TRISC
               MOVLW  0x06         ;EM BINARIO "00000110" DESHABILITA INPUT ANALOGITAS
               MOVWF  ADCON1       ;TODO EL PUERTO A CONFIGURADO COMO I/O DIGITAL.
               BCF    STATUS,RP0   ;SELECCIONAR BANCO 0, RP0 = 0.
               
;========================LIMPIEZA DE REGISTROS ANTES DE EMPEZAR========


      CLRF      SEG_UNIDADES    
      CLRF      SEG_DECENAS      

      CLRF      MIN_UNIDADES     
      CLRF      MIN_DECENAS      
  
      CLRF      HORA_UNIDADES     
      CLRF      HORA_DECENAS    

;=============================================================
;                    RUTINA QUE LEE LOS PUSH PUERTO A
;===============================================================


 
;-----------------------------------------------------------------------
PRINCIPAL
        CLRF BARRIDO 
       
;-------------------------------------------------------------------------

   ;CHECA_PUSH_SET_ALARMA                      ;VERIFICA PUSH BOTON

         BTFSS   PORTA,0                      ;REINICIO  ;PUSH =1?
         CALL    INI_REG
         CALL    ALARMA
        
;-----------------------------------------------------------------------
  ;CHECA_PUSH_MINUTOS                        ;VERFICIA PUSH BOTON INCREMENTO MINUTOS
        BTFSC    PORTA,1;MINUTOS             ;MINUTO  PUSH = 0?
        CALL    SOLO_MINUTOS                 ;SI PUSH BOTON=1 SALTA SIG. SUB RUTINA
     
;---------------------------------------------------------------

  ;CHECA_PUSH_HORA                       
         BTFSC    PORTA,2;HORA            ;VERIFICA PUSH BOTON INCREMENTO HORA
         CALL     SOLO_HORAS              ; PUSH=0? SALTA SIG. SUB RUTINA
         CALL     VALIDAR_TIEMPO          ;VERIFICA LAS 24 HORAS Y PONE A CERO LAS VAR. DE TIEMMPO
         CALL     VALIDAR_ALARMA          ;VERIFICA ESTADO DE ALARMA
         CALL     CONTEOS                 ;LLAMA SUB RUTINA CONTEOS PARA QUE INCREMENTEN LOS SEGUNDOS (RELOJ)
         GOTO     PRINCIPAL;              ;VUELVE AL CICLO PRINCIPAL
;----------------------------------------------------------------       

SOLO_MINUTOS
         CALL AUMENTO_MIN_UNO           ;INCREMENTA EN 1 MINUTO CADA VEZ QUE SE OPRIME EL BOTON
         RETURN
;----------------------------------------------------------------    
  SOLO_HORAS
        CALL AUMENTO_HORA_UNO           ;INCREMENTA EN 1 HORA CADA VEZ QUE SE OPRIME EL BOTON
       RETURN



;=============================================================
;                   RUTINA BASE DE CONTEO SEGUNDOS (RELOJ)
;===============================================================


CONTEOS
AUMENTO_SEG_UNO
         
         INCF    SEG_UNIDADES,F        ;INCREMENTA EL VALOR DEL REGISTRO EN 1
         MOVFW   SEG_UNIDADES          ;CARGA EL VALOR DEL REGISTRO A W  
         SUBLW   D'10'                 ;RESTA 10 PARA VERIFICAR SI HAY ACARREO     
         BTFSC   STATUS,Z              ;SI ES CERO LA RESTA LA BANDERA Z=1
         CALL    AUMENTO_SEG_DIEZ      ;COMO ES CERO SE LLEVA EL ACARREO DE 1
         CALL    BARRIDO
         RETURN

;=======================================================================

AUMENTO_SEG_DIEZ
         CLRF    SEG_UNIDADES          ;SE LIMPIA EL REGISTRO QUE LLEVA EL ACARREO
         INCF    SEG_DECENAS,F         ;INCREMENTA EL VALOR DE DECENAS DE SEGUNDO 
         MOVFW   SEG_DECENAS           ;CARGA EL VALOR DEL REGISTRO A W  
         SUBLW    D'6'                  ;RESTA 10 PARA VERIFICAR SI HAY ACARREO     
         BTFSC   STATUS,Z              ;SI ES CERO LA RESTA LA BANDERA Z=1
         CALL    AUMENTO_MIN_UNO       ;COMO ES CERO SE LLEVA EL ACARREO DE 1
         RETURN 

;=========================================================================
AUMENTO_MIN_UNO
         CLRF    SEG_DECENAS           ;SE LIMPIA EL REGISTRO
         INCF    MIN_UNIDADES,F        ;  INCREMENTA EL VALOR DEL REGISTRO EN 1
         MOVFW   MIN_UNIDADES          ;CARGA EL VALOR DEL REGISTRO A W  
         SUBLW   D'10'                 ;RESTA 10 PARA VERIFICAR SI HAY ACARREO     
         BTFSC   STATUS,Z              ;SI ES CERO LA RESTA LA BANDERA Z=1
         CALL    AUMENTO_MIN_DIEZ      ;COMO ES CERO SE LLEVA EL ACARREO DE 1
         RETURN

;=================================================
AUMENTO_MIN_DIEZ
         CLRF    MIN_UNIDADES          ;SE LIMPIA EL REGISTRO QUE LLEVA EL ACARREO
         INCF    MIN_DECENAS,F         ;INCREMENTA EL VALOR DE DECENAS DE SEGUNDO 
         MOVFW   MIN_DECENAS           ;CARGA EL VALOR DEL REGISTRO A W  
         SUBLW   D'6'                  ;RESTA 10 PARA VERIFICAR SI HAY ACARREO     
         BTFSC   STATUS,Z              ;SI ES CERO LA RESTA LA BANDERA Z=1
         CALL    AUMENTO_HORA_UNO       ;COMO ES CERO SE LLEVA EL ACARREO DE 1
         RETURN  

;=========================================================================
AUMENTO_HORA_UNO
         CLRF    MIN_DECENAS           ;LIMPIA REGISTRO POR ACARREO
         INCF    HORA_UNIDADES,F        ;  INCREMENTA EL VALOR DEL REGISTRO EN 1
         MOVFW   HORA_UNIDADES          ;CARGA EL VALOR DEL REGISTRO A W  
         SUBLW   D'10'                 ;RESTA 10 PARA VERIFICAR SI HAY ACARREO     
         BTFSC   STATUS,Z              ;SI ES CERO LA RESTA LA BANDERA Z=1
         CALL    AUMENTO_HORA_DIEZ      ;COMO ES CERO SE LLEVA EL ACARREO DE 1
         ;MOVLW    0x00 
         ;MOVWF    ADDR_L
         ;CALL    LEER_EEPROM
         ;MOVFW   HORA_UNIDADES_AL 
         ;MOVWF   PORTC
         ;sublw   D'1'
         ;BTFSC   STATUS,Z 
         ;BSF     PORTC,0
         RETURN

;=================================================
AUMENTO_HORA_DIEZ
         CLRF    HORA_UNIDADES          ;SE LIMPIA EL REGISTRO QUE LLEVA EL ACARREO
         INCF    HORA_DECENAS,F         ;INCREMENTA EL VALOR DE DECENAS DE SEGUNDO 
         MOVFW   HORA_DECENAS           ;CARGA EL VALOR DEL REGISTRO A W  
         SUBLW   D'6'                   ;RESTA 10 PARA VERIFICAR SI HAY ACARREO     
         BTFSC   STATUS,Z               ;SI ES CERO LA RESTA LA BANDERA Z=1
         CLRF    HORA_DECENAS           ;SE LIMPIA REGISTRO
         RETURN  


;----------------------DISPLAY #1--------------------------------
BARRIDO 
        
        
;UNIDADES DE SEGUNDO
         MOVLW     B'00001111' 
         MOVWF     PORTB
         MOVFW     SEG_UNIDADES
         IORLW      B'00000000' 
         MOVWF     PORTB
         CALL      PAUSA
         MOVLW     B'00001111' 
         MOVWF     PORTB
;-----------------------------DISPLAY #2--------------------------------
;DECENAS DE SEGUNDO

      MOVLW     B'00011111'                     ; SE ACTIVA DISPLAY # 2
      MOVWF     PORTB                         ; SE CARGA EL DATO AL PUERTO B
      MOVFW      SEG_DECENAS                  ;MUEVE AL RESGITRO W
      IORLW      B'00010000'                    ; Aplicamos la máscara
      MOVWF     PORTB                        ; SE MUESTRA EN PUERTO B
      CALL      PAUSA                           ; 
      MOVLW     B'00011111'                     ; SE CARGA W PARA LIMPIEZA DE REGISTROS
      MOVWF     PORTB                         ; PA


;-----------------------------DISPLAY #3--------------------------------
;UNIDADES DE MINUTO

      MOVLW     B'00101111'                     ; SE ACTIVA DISPLAY # 3
      MOVWF     PORTB                         ; SE CARGA EL DATO AL PUERTO B
      MOVFW      MIN_UNIDADES                  ;MUEVE AL RESGITRO W
      IORLW      B'00100000'                    ; Aplicamos la máscara
      MOVWF     PORTB                         ; SE MUESTRA EN PUERTO B
      CALL      PAUSA                           ; 
      MOVLW      B'00101111'                     ; SE CARGA W PARA LIMPIEZA DE REGISTROS
      MOVWF     PORTB                         ; PARA EVITAR QUE SE REPITAN



;-----------------------------DISPLAY #4--------------------------------
;DECENAS DE MINUTO

      MOVLW     B'00111111'                     ; SE ACTIVA DISPLAY # 4
      MOVWF     PORTB                        ; SE CARGA EL DATO AL PUERTO B
      MOVFW      MIN_DECENAS                 ;MUEVE AL RESGITRO W
      IORLW      B'00110000'                    ; Aplicamos la máscara
      MOVWF     PORTB                         ; SE MUESTRA EN PUERTO B
      CALL      PAUSA                           ; 
      MOVLW     B'00111111'                     ; SE CARGA W PARA LIMPIEZA DE REGISTROS
      MOVWF     PORTB            

;-----------------------------DISPLAY #5--------------------------------
;UNIDADES DE HORA

      MOVLW     B'01001111'                     ; SE ACTIVA DISPLAY # 5
      MOVWF     PORTB                         ; SE CARGA EL DATO AL PUERTO B
      MOVFW      HORA_UNIDADES                  ;MUEVE AL RESGITRO W
      IORLW      B'01000000'                    ; Aplicamos la máscara
      MOVWF     PORTB                         ; SE MUESTRA EN PUERTO B
      CALL      PAUSA                           ; 
      MOVLW     B'01001111'                      ; SE CARGA W PARA LIMPIEZA DE REGISTROS
      MOVWF     PORTB          

;-----------------------------DISPLAY #6--------------------------------
;DECENAS DE HORA

      MOVLW     B'01011111'                     ; SE ACTIVA DISPLAY # 6
      MOVWF     PORTB                         ; SE CARGA EL DATO AL PUERTO B
      MOVFW      HORA_DECENAS                  ;MUEVE AL RESGITRO W
      IORLW      B'01010000'                    ; Aplicamos la máscara
      MOVWF     PORTB                         ; SE MUESTRA EN PUERTO B
      CALL      PAUSA                          ; 
      MOVLW     B'01011111'                      ; SE CARGA W PARA LIMPIEZA DE REGISTROS
      MOVWF     PORTB  


RETURN


;-------------------------------------------------------------------------------------

 INI_REG
     ;;REGISTROS EN CERO
      CLRF      SEG_UNIDADES    
      CLRF      SEG_DECENAS      

      CLRF      MIN_UNIDADES     
      CLRF      MIN_DECENAS      
  
      CLRF      HORA_UNIDADES     
      CLRF      HORA_DECENAS    
      RETURN 

;------------------------------------------------------------------------------------

;VALIDAR SI SE LLEGO A LAS 24 HORAS
VALIDAR_TIEMPO
      MOVFW      HORA_UNIDADES                   ;CARGA EL REGISTRO W CON EL VALOR DE HORAS
      SUBLW      D'4'                            ;RESTA 4 PARA VERIFICAR SI W=4
      BTFSC      STATUS,Z                        ;SE VERIFICA SI EL RESULTADO FUE CERO 
      GOTO       VALIDAR_HORA
      RETURN
;----------------------------------------------------------------------------------

VALIDAR_HORA
      MOVFW      HORA_DECENAS                   ;CARGA EL REGISTRO W CON LO QUE ESTA GUARDAD EN HORAS_DECENAS
      SUBLW      D'2'                           ; RESTA 2 PARA VERIFICAR SI W=2
      BTFSC      STATUS,Z                       ;VERIFICA SI EL RESULTADO ES CERO 
      GOTO       INI_REG
      RETURN

;=============================================================
;                   SUB RUTINA DE VALIDACION ALARMA
;=============================================================

VALIDAR_ALARMA                                

     BTFSC      PORTA,4                       ;SI PUSH=0? ACTIVA ALARMA
     CALL      M_U_AL                         ;SI PUSH=1 , SALTA A SIG. SUBRUTINA
     MOVLW     0X00
     MOVWF     PORTC
     RETURN                                   ;SI NO ESTA ACTIVADA REGRESA
;----------------------------------------------------------------    

;SI SE CUMPLEN LAS DOS CONDICIONES: ( MIN_AL = MIN && HORA_AL = HORA ) ACTIVA BOCINA
  M_U_AL   
     MOVFW     MIN_UNIDADES
     XORLW     D'0'                           ;MIN_UNIDADES_AL
     BTFSC     STATUS,Z 
     CALL     M_D_AL
     RETURN
;----------------------------------------------------------------    
   M_D_AL 
 
     MOVFW     MIN_DECENAS
     XORLW     D'3'                          ;MIN_DECENAS_AL
     BTFSC     STATUS,Z 
     CALL      H_U_AL
     RETURN
;----------------------------------------------------------------    
   H_U_AL

     MOVFW     HORA_UNIDADES
     SUBLW     D'0'                          ;HORA_UNIDADES_AL
     BTFSC     STATUS,Z 
     CALL      H_D_AL
     RETURN
;----------------------------------------------------------------    

   H_D_AL

     MOVFW     HORA_DECENAS
     SUBLW     D'1'                         ;HORA_DECENAS_AL
     BTFSC     STATUS,Z 
     CALL      ACTIVA_BOCINA
     RETURN

;----------------------------------------------------------------    
   ACTIVA_BOCINA
      BTFSC     PORTA,5                     ;BOTON SNOOZE
      CALL      SNOOZE
      
      MOVLW     0X00                        ;APAGA BOCINA ALARMA
      MOVWF     PORTC
      CALL      PAUSA
    
      MOVLW     0X01                        ;ENCIENDE BOCINA ALARMA
      MOVWF     PORTC
      CALL      PAUSA 
      RETURN
  SNOOZE
      MOVLW     0X00                       ;APAGA BOCINA 
      MOVWF     PORTC                      ;PONE A CERO PUERTO C
      CALL     RETARDO_SNOOZE              ;LLAMA RETARDO DE 120 SEG  (2MIN)
      CALL     RETARDO_SNOOZE              ;LLAMA RETARDO DE 120 SEG (4MIN)
      CALL     RETARDO_SNOOZE              ;LLAMA RETARDO DE 120 SEG (6MIN) 
      CALL     RETARDO_SNOOZE              ;LLAMA RETARDO DE 120 SEG (8MIN) 
      CALL     RETARDO_SNOOZE              ;LLAMA RETARDO DE 120 SEG (10MIN) 
      
      RETURN
      


;======================================================================================
 ;                          SUB-RUTINA DE ALARMA
;===========================================================================================
 ALARMA
;-------------------------------------------------------------------------
         BTFSC   PORTA,3                     ; SE CHECA SI PUSH SET ALALRMA ESTA ACTIVADO
         GOTO    CHECA_PUSH_MINUTOS          ; SI ESTA METIDO PUSH, INGRESA DATO DE MINUTOS ALARMA
         CALL    CONTEOS_AL                  ; LLAMA SUBRUTINA 
         RETURN
        
   CHECA_PUSH_MINUTOS                        ; ALARMA VERFICIA PUSH BOTON
                        
         BTFSC    PORTA,1                    ; MIN _AL PUSH = 0?
         CALL    SOLO_MIN_ALARMA
               
;-----------------------------------------------------------------------
        ;CHECA_PUSH_HORA_AL                  ;VERIFICA PUSH BOTON HORA ESTA PRESIONADO
          BTFSC    PORTA,2                   ;HORA_AL PUSH = 0?
          CALL     SOLO_HORAS_AL             ;PASA A ESTA LINEA SI PUSH =1,SALTA A SIG. SUBRUTINA
          CALL     VALIDAR_TIEMPO_AL
          CALL     BARRIDO_AL
          GOTO     ALARMA
        
;----------------------------------------------------------------------------------
     SOLO_MIN_ALARMA
           CALL      AUMENTO_MIN_UNO_AL 
           RETURN
        
;---------------------------------------------------------------

  SOLO_HORAS_AL
        CALL       AUMENTO_HORA_UNO_AL 
         RETURN 


;---------------------------------------------------------------

CONTEOS_AL 
         
        
   AUMENTO_MIN_UNO_AL
         
         BTFSC   PORTA,3
         INCF    MIN_UNIDADES_AL,F        ;  INCREMENTA EL VALOR DEL REGISTRO EN 1
         MOVFW   MIN_UNIDADES_AL          ;CARGA EL VALOR DEL REGISTRO A W  
         BTFSC   PORTA,3
          SUBLW   D'10'                    ;RESTA 10 PARA VERIFICAR SI HAY ACARREO     
          BTFSC   STATUS,Z                 ;SI ES CERO LA RESTA LA BANDERA Z=1
          CALL    AUMENTO_MIN_DIEZ_AL      ;COMO ES CERO SE LLEVA EL ACARREO DE 1
          BTFSC   PORTA,3
          CALL    BARRIDO_AL
          BTFSC   PORTA,3
          RETURN
          CALL    AUMENTO_MIN_DIEZ_AL
          RETURN

;=================================================
   AUMENTO_MIN_DIEZ_AL
         BTFSC   PORTA,3
         CLRF    MIN_UNIDADES_AL          ;SE LIMPIA EL REGISTRO QUE LLEVA EL ACARREO
         BTFSC   PORTA,3
         INCF    MIN_DECENAS_AL,F         ;INCREMENTA EL VALOR DE DECENAS DE SEGUNDO 
         MOVFW   MIN_DECENAS_AL           ;CARGA EL VALOR DEL REGISTRO A W  
         BTFSC   PORTA,3
         SUBLW   D'6'                     ;RESTA 10 PARA VERIFICAR SI HAY ACARREO     
         BTFSC   STATUS,Z                 ;SI ES CERO LA RESTA LA BANDERA Z=1
         CALL    AUMENTO_HORA_UNO_AL       ;COMO ES CERO SE LLEVA EL ACARREO DE 1
         BTFSC   PORTA,3
         CALL    BARRIDO_AL
         BTFSC   PORTA,3
         RETURN
         CALL    AUMENTO_HORA_UNO_AL
         RETURN  
      
      
;=========================================================================
   AUMENTO_HORA_UNO_AL
         BTFSC   PORTA,3                  
         CLRF    MIN_DECENAS_AL           ;LIMPIA REGISTRO POR ACARREO
         BTFSC   PORTA,3
         INCF    HORA_UNIDADES_AL,F        ;  INCREMENTA EL VALOR DEL REGISTRO EN 1
         MOVFW   HORA_UNIDADES_AL          ;CARGA EL VALOR DEL REGISTRO A W  
         BTFSC   PORTA,3
         SUBLW   D'10'                     ;RESTA 10 PARA VERIFICAR SI HAY ACARREO     
         BTFSC   STATUS,Z                  ;SI ES CERO LA RESTA LA BANDERA Z=1
         CALL    AUMENTO_HORA_DIEZ_AL      ;COMO ES CERO SE LLEVA EL ACARREO DE 1
         BTFSC   PORTA,3
         RETURN
         CALL    AUMENTO_HORA_DIEZ_AL
         RETURN

;=================================================
AUMENTO_HORA_DIEZ_AL
         BTFSC   PORTA,3
         CLRF    HORA_UNIDADES_AL          ;SE LIMPIA EL REGISTRO QUE LLEVA EL ACARREO
         BTFSC   PORTA,3
         INCF    HORA_DECENAS_AL,F         ;INCREMENTA EL VALOR DE DECENAS DE SEGUNDO 
         MOVFW   HORA_DECENAS_AL           ;CARGA EL VALOR DEL REGISTRO A W  
         BTFSC   PORTA,3
         SUBLW   D'6'                      ;RESTA 10 PARA VERIFICAR SI HAY ACARREO     
         BTFSC   STATUS,Z                  ;SI ES CERO LA RESTA LA BANDERA Z=1
         CLRF    HORA_DECENAS_AL           ;SE LIMPIA REGISTRO
         BTFSC   PORTA,3
         CALL    BARRIDO_AL
         RETURN  


;======================================================================
;SUBRUTINA PARA LEER 6 DISPLAY ALARMA
;======================================================================
  
  BARRIDO_AL
  
;NO SE USAN LOS DISPLAY DE SEGUNDOS EN EL MODO ALARMA       
;-----------------------------DISPLAY #1--------------------------------
;UNIDADES DE SEGUNDO

       ;MOVLW     B'00001111'                 ; SE ACTIVA DISPLAY # 1
       ;MOVWF     PORTB                       ; SE CARGA EL DATO AL PUERTO B
       ;MOVFW     B'00000000'                 ;MUEVE AL RESGITRO W
      ; MOVWF     PORTB                       ; SE MUESTRA EN PUERTO B
      ; CALL      PAUSA                       ; 
      ; MOVLW     B'00001111'                 ; SE CARGA W PARA LIMPIEZA DE REGISTROS
      ; MOVWF     PORTB                       ; PARA EVITAR QUE SE REPITAN

;-----------------------------DISPLAY #2--------------------------------
;DECENAS DE SEGUNDO

     ; MOVLW     B'00011111'                   ; SE ACTIVA DISPLAY # 2
     ; MOVWF     PORTB                         ; SE CARGA EL DATO AL PUERTO B
     ; MOVFW      B'00000000'                  ;MUEVE AL RESGITRO W
     ; MOVWF      PORTB                        ; SE MUESTRA EN PUERTO B
     ; CALL      PAUSA                         ; 
     ; MOVLW     B'00011111'                   ; SE CARGA W PARA LIMPIEZA DE REGISTROS
     ; MOVWF      PORTB                        ; PARA EVITAR QUE SE REPITAN

;-----------------------------DISPLAY #3--------------------------------
 ;UNIDADES DE MINUTO

      MOVLW     B'00101111'                    ; SE ACTIVA DISPLAY # 3
      MOVWF      PORTB                         ; SE CARGA EL DATO AL PUERTO B
      MOVFW      MIN_UNIDADES_AL               ;MUEVE AL RESGITRO W
      IORLW      B'00100000'                   ; Aplicamos la máscara
      MOVWF      PORTB                         ; SE MUESTRA EN PUERTO B
      CALL      PAUSA                          ; 
      MOVLW      B'00101111'                   ; SE CARGA W PARA LIMPIEZA DE REGISTROS
      MOVWF      PORTB                         ; PARA EVITAR QUE SE REPITAN

;-----------------------------DISPLAY #4--------------------------------
;DECENAS DE MINUTO
;
      MOVLW     B'00111111'                    ; SE ACTIVA DISPLAY # 4
      MOVWF      PORTB                         ; SE CARGA EL DATO AL PUERTO B
      MOVFW      MIN_DECENAS_AL                ;MUEVE AL RESGITRO W
      IORLW      B'00110000'                   ; Aplicamos la máscara
      MOVWF      PORTB                         ; SE MUESTRA EN PUERTO B
      CALL       PAUSA                          ; 
      MOVLW      B'00111111'                    ; SE CARGA W PARA LIMPIEZA DE REGISTROS
      MOVWF      PORTB            

;-----------------------------DISPLAY #5--------------------------------
;UNIDADES DE HORA

      MOVLW     B'01001111'                    ; SE ACTIVA DISPLAY # 5
      MOVWF      PORTB                         ; SE CARGA EL DATO AL PUERTO B
      MOVFW      HORA_UNIDADES_AL              ;MUEVE AL RESGITRO W
      MOVWF    DATA_L 
      MOVLW    0x00
      MOVWF    ADDR_L
      CALL    ESCRIBIR_EEPROM
      MOVFW      HORA_UNIDADES_AL
      IORLW      B'01000000'                   ; Aplicamos la máscara
      MOVWF      PORTB                         ; SE MUESTRA EN PUERTO B
      CALL       PAUSA                          ; 
      MOVLW     B'01001111'                    ; SE CARGA W PARA LIMPIEZA DE REGISTROS
      MOVWF      PORTB         

;-----------------------------DISPLAY #6--------------------------------
;DECENAS DE HORA

      MOVLW     B'01011111'                    ; SE ACTIVA DISPLAY # 6
      MOVWF      PORTB                         ; SE CARGA EL DATO AL PUERTO B
      MOVFW      HORA_DECENAS_AL               ;MUEVE AL RESGITRO W
      IORLW      B'01010000'                   ; Aplicamos la máscara
      MOVWF      PORTB                         ; SE MUESTRA EN PUERTO B
      CALL       PAUSA                          ; 
      MOVLW      B'01011111'                    ; SE CARGA W PARA LIMPIEZA DE REGISTROS
      MOVWF     PORTB   

     RETURN

;-------------------------INICIAR REGISTROS CUANDO SE LLEGA A 24 HORAS-----------------

 INI_REG_AL
     ;;REGISTROS EN CERO
      CLRF      MIN_UNIDADES_AL     
      CLRF      MIN_DECENAS_AL      
  
      CLRF      HORA_UNIDADES_AL     
      CLRF      HORA_DECENAS_AL    
      RETURN 

;------------------------------------------------------------------------------------

;VALIDAR SI SE LLEGO A LAS 24 HORAS
VALIDAR_TIEMPO_AL
      MOVFW      HORA_UNIDADES_AL                   ;CARGA EL REGISTRO W CON EL VALOR DE HORAS
      SUBLW      D'4'                            ;RESTA 4 PARA VERIFICAR SI W=4
      BTFSC      STATUS,Z                        ;SE VERIFICA SI EL RESULTADO FUE CERO 
      GOTO       VALIDAR_HORA_AL
      RETURN
;----------------------------------------------------------------------------------

VALIDAR_HORA_AL
      MOVFW      HORA_DECENAS_AL                   ;CARGA EL REGISTRO W CON LO QUE ESTA GUARDAD EN HORAS_DECENAS
      SUBLW      D'2'                           ; RESTA 2 PARA VERIFICAR SI W=2
      BTFSC      STATUS,Z                       ;VERIFICA SI EL RESULTADO ES CERO 
      GOTO       INI_REG_AL
      RETURN

;==============GRABAR DATOS EN EEPROM====================
;=======================================================

	ESCRIBIR_EEPROM
	 bcf  STATUS,RP0                               ;Ir banco 0
	 bcf  STATUS,RP1
	 movfw ADDR_L                                  ;;EEADR = ADDR_L
	 bcf  STATUS,RP0                               ;Ir banco 2
	 bsf  STATUS,RP1
	 movwf EEADR
	 bcf  STATUS,RP0                               ;Ir banco 0
	 bcf  STATUS,RP1
	 movfw DATA_L                                  ;;EEDATA = DATA_L
	 bcf  STATUS,RP0                               ;Ir banco 2
	 bsf  STATUS,RP1
	 movwf EEDATA
	 bsf  STATUS,RP0                              ;Ir banco 3
	 bsf  STATUS,RP1
	 bcf  EECON1,EEPGD                            ;Apuntar a la memoria EEPROM
	 bsf  EECON1,WREN                             ;Habilitar escritura
	 bcf  INTCON,GIE                              ;Deshabilita interrupciones
	 movlw 55h
	 movwf EECON2                                 ;Escribe 55 hexadecimal
	 movlw 0xAA
	 movwf EECON2                                 ;Escribe AA hexadecimal
	 bsf   EECON1,WR                              ;Habilita el bit de escritura
	 ; bsf INTCON,GIE                             ;Habilita interrupciones
	_bucle1
	 btfsc EECON1,WR                              ;Espera el final de grabación
	 goto _bucle1                                 ;Si no termina la grabación: Ir _bucle
	 bcf   PIR2,EEIF                              ;Si termina Borra bandera de interrupción
	 bcf   EECON1,WREN                            ;Deshabilitar escritura
	 bcf   STATUS,RP0                             ;Ir banco 0
	 bcf   STATUS,RP1
	 return                                       ;Retorno

;====================================================================
;           RUTINA PARA LEER DATOS GRABADOS EN EEPROM
;====================================================================

	LEER_EEPROM
	 bcf  STATUS,RP0                             ;Ir banco 0
	 bcf  STATUS,RP1
	 movfw ADDR_L                                ;Cargar dirección a leer
	 bcf  STATUS,RP0                             ;Ir banco 2
	 bsf  STATUS,RP1
	 movwf EEADR
	 bsf  STATUS,RP0                             ;Ir banco 3
	 bsf  STATUS,RP1
	 bcf  EECON1,EEPGD                           ;Apunta a la memoria EEPROM
	 bsf  EECON1,RD                              ;Habilita ciclo de lectura
	 bcf  STATUS,RP0                             ;Ir banco 2
	 bsf  STATUS,RP1
	 movfw EEDATA                                ;W = EEDATA (leer dato de EEPROM)
	 bcf  STATUS,RP0                             ;Ir banco 0
	 bcf  STATUS,RP1
	 movwf DATA_L                               ;DATA_L = W (almacena dato de EEPROM)
	 return                                     ;Retorno



;=============================================================
;                   RUTINA DE RETARDO (DELAY)
;===============================================================

PAUSA
;PIC Time Delay = 0.30000100 s with Osc = 4000000 Hz
		movlw	D'2'
		movwf	CONTADOR2
		movlw	D'134'
		movwf	CONTADOR3
		movlw	D'152'
		movwf	CONTADOR4
loop		decfsz	CONTADOR4,1
		goto	loop
		decfsz	CONTADOR3,1
		goto	loop
		decfsz	CONTADOR2,1
		goto	loop
		retlw	0
;==============================================================
;
;==============================================================
RETARDO_SNOOZE
;PIC Time Delay = 120.00000000 s with Osc = 4000000 Hz
		movlw	D'3'
		movwf	CONTADOR5
		movlw	D'97'
		movwf	CONTADOR6
		movlw	D'195'
		movwf	CONTADOR7
		movlw	D'141'
		movwf	CONTADOR8
loop1		decfsz	CONTADOR8,1
		goto	loop1
		decfsz	CONTADOR7,1
		goto	loop1
		decfsz	CONTADOR6,1
		goto	loop1
		decfsz	CONTADOR5,1
		goto	loop1
		retlw	0



	END