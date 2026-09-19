  IDENTIFICATION DIVISION.                                         
 PROGRAM-ID. BANC01.                                              
*------------------------------------------------------------*    
* PROGRAMA  : BANC01                                         *    
* FUNCION   : MENU PRINCIPAL BANCO - CONSULTAS Y OPERACIONES *    
* TRANSACCION: BA01                                          *    
* MAPA      : BANC  (MAPSET BANC)                            *    
* COMMAREA  : CA-COMMAREA (COMAREA)                          *    
*------------------------------------------------------------*    
 DATA DIVISION.                                                   
 WORKING-STORAGE SECTION.                                         
*--- DCLGEN DE TABLAS ----*                                       
                                                                  
       EXEC SQL                                                   
           INCLUDE SQLCA                                          
       END-EXEC.                                                  
                                                                  
       EXEC SQL                                                   
           INCLUDE CLIENTE                                        
       END-EXEC.                                                  
       EXEC SQL                                                   
           INCLUDE CUENTA                                         
       END-EXEC.                                                  
       EXEC SQL                                                   
           INCLUDE MOVIMIEN                                       
       END-EXEC.                                                  
       EXEC SQL                                                   
           INCLUDE RETIRADA                                       
       END-EXEC.                                                  
       EXEC SQL                                                   
           INCLUDE INGRESO                                        
       END-EXEC.                                                  
*-- MAPA BMS -----*                                               
       COPY BANC.                                                 
       COPY DFHAID.                                               
*----- VARIABLES DE TRABAJO ------*                               
 01  WS-VARIABLES.                                                
       05  WS-RESP   
      PIC S9(08) COMP.                  
            05 WS-COMMAREA-INIT      PIC X(55) VALUE LOW-VALUES.       
            05  WS-CUENTA-FIJA       PIC X(10) VALUE '0000000001'.     
           05  WS-IMPORTE-FIJO      PIC S9(10)V99 COMP-3  VALUE 100.0. 
            05  WS-SQLCODE-EDIT      PIC 9(09).                        
            05  WS-SALDO-EDIT        PIC 9(09)99.                      
            05  WS-CONTADOR          PIC S9(04) COMP.                  
            05  WS-FECHA-HOY         PIC X(10).                        
     *--- INDICADORES NULL PARA DB2 ------*                            
      01  WS-INDICADORES.                                              
            05  IND-NUM-CUENTA       PIC S9(04) COMP.                  
            05  IND-SALDO            PIC S9(04) COMP.                  
            05  IND-DIVISA           PIC S9(04) COMP.                  
            05  WS-SALDO-TXT         PIC X(12).                        
            05  WS-SQLCODE-TXT       PIC X(09).                        
      LINKAGE SECTION.                                                 
      01 DFHCOMMAREA.                                                  
         05 CA-ESTADO-CST  PIC X(02).                                  
         05 CA-OPCION-SELECC PIC X(02).                                
         05 CA-DATOS-SESION.                                           
            10 CA-NUMERO-CUENTA PIC X(10).                             
            10 CA-SALDO-ACTUAL  PIC S9(7)V99 COMP-3.                   
         05 FILLER    PIC X(36).                                       
      PROCEDURE DIVISION.                                              
     *-----------------------------------------*                       
     * PUNTO DE ENTRADA                        *                       
     *-----------------------------------------*                       
      0000-PRINCIPAL.                                                  
            EVALUATE EIBCALEN                                          
                WHEN 0                                                 
                    PERFORM 1000-INICIALIZAR                           
                    PERFORM 8000-ENVIAR-MAPA                           
                WHEN OTHER                                             
                    PERFORM 2000-RECIBIR-MAPA                          
                    PERFORM 3000-PROCESAR-OPCION                       
                    PERFORM 8000-ENVIAR-MAPA                           
            END-EVALUATE.                                              
            IF EIBCALEN = 0                                            
            EXEC CICS                                                  
                 RETURN TRANSID('BA01')          
                        COMMAREA(WS-COMMAREA-INIT)                     
                       LENGTH(55)                                     
           END-EXEC                                                   
           ELSE                                                       
            EXEC CICS                                                 
                RETURN TRANSID('BA01')                                
                   COMMAREA(DFHCOMMAREA)                              
                   LENGTH(LENGTH OF DFHCOMMAREA)                      
            END-EXEC                                                  
            END-IF.                                                   
                                                                      
            GOBACK.                                                   
    *--------------------------------------------*                    
    * INICIALIZAR COMMAREA Y MAPA                *                    
    *--------------------------------------------*                    
     1000-INICIALIZAR.                                                
          MOVE LOW-VALUES TO MAP1O                                    
          MOVE 'BANCO NESTOR - MENU PRINCIPAL' TO SUBTITO             
          MOVE 'SELECCIONE UNA OPCION (01-04):' TO SEL_LBLO           
          MOVE '01 - CONSULTAR SALDO Y MOVIMIENTOS' TO M_OPC1O        
          MOVE '02 - RETIRAR EFECTIVO'              TO M_OPC2O        
          MOVE '03 - INGRESAR DINERO'               TO M_OPC3O        
          MOVE '04 - OTRAS OPERACIONES'             TO M_OPC4O        
          MOVE 'ESCRIBA LA OPCION Y PULSE ENTER'   TO AYUDAO          
          MOVE SPACES TO MENSAJEO                                     
          MOVE SPACES TO SEL_VALO.                                    
    *------------------------------------*                            
    * RECIBIR MAPA                       *                            
    *------------------------------------*                            
     2000-RECIBIR-MAPA.                                               
          EXEC CICS                                                   
           RECEIVE MAP('MAP1')                                        
                   MAPSET('BANC')                                     
                   INTO(MAP1I)                                        
                   RESP(WS-RESP)                                      
          END-EXEC.                                                   
                                                                      
          IF EIBAID = DFHPF3                                          
             EXEC CICS          
                   SEND CONTROL FREEKB ERASE                         
              END-EXEC                                               
              EXEC CICS                                              
                  RETURN                                             
              END-EXEC                                               
           END-IF.                                                   
                                                                     
           IF WS-RESP = DFHRESP(NORMAL)                              
                 MOVE SEL_VALI TO CA-OPCION-SELECC                   
           ELSE                                                      
                 MOVE SPACES TO CA-OPCION-SELECC                     
           END-IF.                                                   
     *--------------------------------------*                        
     * PROCESAR LA OPCION ELEGIDA           *                        
     *--------------------------------------*                        
      3000-PROCESAR-OPCION.                                          
            DISPLAY '=== 3000-PROCESAR-OPCION ==='.                  
            DISPLAY 'SEL_VALI: [' SEL_VALI ']'.                      
            DISPLAY 'LONGITUD: ' LENGTH OF SEL_VALI.                 
           EVALUATE SEL_VALI                                         
            WHEN '01'                                                
               PERFORM 4000-CONSULTAR                                
            WHEN '02'                                                
               PERFORM 5000-RETIRAR                                  
            WHEN '03'                                                
               PERFORM 6000-INGRESAR                                 
            WHEN '04'                                                
               PERFORM 7000-OTRAS                                    
            WHEN OTHER                                               
               MOVE 'OPCION NO VALIDA. INTENTE DE NUEVO.'            
                 TO MENSAJEO                                         
               MOVE SPACES TO SEL_VALO                               
           END-EVALUATE.                                             
     *-----------------------------------------------*               
     * OPCION 01: CONLSULTA DE SALDO Y MOVIMIENTOS   *               
     *-----------------------------------------------*               
      4000-CONSULTAR.                                                
           MOVE WS-CUENTA-FIJA TO CD-CUENTA1.     
            EXEC SQL                                                    
                SELECT NUM_CUENTA, SALDO, DIVISA                        
                INTO :CD-CUENTA1,  :CD-CUENTA3, :CD-CUENTA4             
                FROM IBMUSER.BANC_CUENTA                                
                WHERE NUM_CUENTA = :WS-CUENTA-FIJA                      
            END-EXEC.                                                   
                                                                        
            EVALUATE SQLCODE                                            
                 WHEN 0                                                 
                    MOVE CD-CUENTA3 TO WS-SALDO-EDIT                    
                    MOVE WS-SALDO-EDIT TO WS-SALDO-TXT                  
                    STRING 'CUENTA: ' DELIMITED BY SIZE                 
                           CD-CUENTA1 DELIMITED BY SIZE                 
                           ' SALDO: ' DELIMITED BY SIZE                 
                           WS-SALDO-TXT DELIMITED  BY  SIZE             
                           ' ' DELIMITED BY SIZE                        
                           CD-CUENTA4 DELIMITED BY SIZE                 
                       INTO MENSAJEO                                    
                    END-STRING                                          
                 WHEN 100                                               
                    MOVE ' CUENTA NO ENCONTRADA.' TO MENSAJEO           
                 WHEN OTHER                                             
                    MOVE SQLCODE TO WS-SQLCODE-EDIT                     
                    STRING ' ERROR DB2 EN SONSULTA. SQLCODE:'           
                            DELIMITED BY SIZE                           
                            WS-SQLCODE-EDIT DELIMITED BY SIZE           
                       INTO MENSAJEO                                    
                    END-STRING                                          
            END-EVALUATE.                                               
      *---------------------------------------*                         
      * OPCION 02: RETIRAR EFECTIVO           *                         
      *---------------------------------------*                         
       5000-RETIRAR.                                                    
             MOVE WS-CUENTA-FIJA TO CD-CUENTA1.                         
             EXEC SQL                                                   
                 SELECT SALDO                                           
                 INTO :CD-CUENTA3                                       
                 FROM IBMUSER.BANC_CUENTA                               
                 WHERE NUM_CUENTA = :WS-CUENTA-FIJA    
             END-EXEC.                                                 
            EVALUATE TRUE                                             
              WHEN SQLCODE = 100                                      
                 MOVE 'CUENTA NO ENCONTRADA.' TO MENSAJEO             
            WHEN SQLCODE = 0 AND CD-CUENTA3 <  WS-IMPORTE-FIJO        
                MOVE 'SALDO INSUFICIENTE.' TO MENSAJEO                
            WHEN SQLCODE = 0 AND CD-CUENTA3 >= WS-IMPORTE-FIJO        
             COMPUTE CD-CUENTA3 = CD-CUENTA3 - WS-IMPORTE-FIJO        
            EXEC SQL                                                  
                UPDATE IBMUSER.BANC_CUENTA                            
                SET SALDO = :CD-CUENTA3                               
                WHERE NUM_CUENTA = :WS-CUENTA-FIJA                    
            END-EXEC                                                  
            IF SQLCODE = 0                                            
             MOVE CD-CUENTA3 TO WS-SALDO-EDIT                         
             MOVE WS-SALDO-EDIT TO WS-SALDO-TXT                       
             STRING 'RETIRADA REALIZADA. NUEVO SALDO: '               
                     DELIMITED BY SIZE                                
                     WS-SALDO-TXT DELIMITED BY    SIZE                
                     INTO MENSAJEO                                    
             END-STRING                                               
               ELSE                                                   
             MOVE 'ERROR AL ACTUALIZAR SALDO.' TO MENSAJEO            
               END-IF                                                 
             WHEN OTHER                                               
                MOVE 'ERROR DESCONOCIDO EN RETIRADA.' TO MENSAJEO     
           END-EVALUATE.                                              
     *-----------------------------------*                            
     * OPCION 03: INGRESAR DINERO        *                            
     *-----------------------------------*                            
      6000-INGRESAR.                                                  
             MOVE WS-CUENTA-FIJA TO CD-CUENTA1.                       
             EXEC SQL                                                 
                 SELECT SALDO                                         
                 INTO :CD-CUENTA3                                     
                 FROM IBMUSER.BANC_CUENTA                             
                 WHERE NUM_CUENTA = :WS-CUENTA-FIJA                   
             END-EXEC.                                                
             IF SQLCODE = 0            
                   ADD WS-IMPORTE-FIJO TO CD-CUENTA3                   
              EXEC SQL                                                 
                  UPDATE IBMUSER.BANC_CUENTA                           
                  SET SALDO = :CD-CUENTA3                              
                  WHERE NUM_CUENTA = :WS-CUENTA-FIJA                   
              END-EXEC                                                 
              IF SQLCODE = 0                                           
                  MOVE CD-CUENTA3 TO WS-SALDO-EDIT                     
                  STRING 'INGRESO REALIZADO. NUEVO SALDO: '            
                         DELIMITED BY SIZE                             
                         WS-SALDO-TXT  DELIMITED BY SIZE               
                         INTO MENSAJEO                                 
                  END-STRING                                           
              ELSE                                                     
                   MOVE 'ERROR AL ACTUALIZAR SALDO.' TO MENSAJEO       
              END-IF                                                   
              ELSE                                                     
                   MOVE 'CUENTA NO ENCONTRADA.' TO MENSAJEO            
              END-IF.                                                  
      *----------------------------------------*                       
      * OPCION 04: OTRAS OPERACIONES           *                       
      *----------------------------------------*                       
       7000-OTRAS.                                                     
             MOVE WS-CUENTA-FIJA TO CD-CUENTA1.                        
             EXEC SQL                                                  
                 SELECT SALDO                                          
                 INTO :CD-CUENTA3                                      
                 FROM IBMUSER.BANC_CUENTA                              
                 WHERE NUM_CUENTA = :WS-CUENTA-FIJA                    
             END-EXEC.                                                 
             IF SQLCODE = 0                                            
               MOVE CD-CUENTA3 TO WS-SALDO-EDIT                        
               STRING 'CONSULTA DE OTRAS OPERACIONES. CUENTA:'         
                       DELIMITED BY SIZE                               
                      WS-CUENTA-FIJA DELIMITED BY SIZE                 
                      '  SALDO: ' DELIMITED BY SIZE                    
                      WS-SALDO-EDIT DELIMITED BY SIZE INTO MENSAJEO    
               END-STRING                                              
             ELSE          
                MOVE 'CUENTA NO ENCONTRADA.' TO MENSAJEO               
            END-IF.                                                    
      *------------------------------*                                 
      * ENVIAR MAPA                  *                                 
      *------------------------------*                                 
       8000-ENVIAR-MAPA.                                               
             MOVE SPACES TO SEL_VALO                                   
             EXEC CICS                                                 
                 SEND MAP('MAP1')                                      
                 MAPSET('BANC')                                        
                 FROM(MAP1O)                                           
                 ERASE                                                 
             END-EXEC.                                                  			 
