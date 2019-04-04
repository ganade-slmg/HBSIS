       PROGRAM-ID.   CALCDIGITO.
       AUTHOR.       GANADE.

      ******************************************************************
      * EXECUTA A VALIDACAO DE UM CNPJ / CPF INFORMADO
      * BASEADO EM FONTE ENCONTRADO NA INTERNET
      * http://www.ti4fun.com/corintiano/Rotinas/Cobol?r=KwgHblzCoac[[ti
      ******************************************************************
      *  OBJETIVO      : VERIFICA O DIGITO DO CPF CNPJ OU PIS/PSASEP
      *  ANALISTA      : CARLOS ALBERTO DORNELLES
      *  COMO USAR     : LKS-NUMERO-I ....: NUMERO INFORMADO
      *                : LKS-NUMERO-F ....: NUMERO CALCULADO
      *                : LKS-TIPO-CALCULO : CPF, CGC OU PIS
      *                : LKS-ACAO ........: C - CALCULA
      *                                     V - VERIFICA
      ******************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01  WS-AUXILIARES.
           05 WSS-IND-N                  PIC 9(002)  VALUE ZEROES.
           05 WSS-IND-O                  PIC 9(002)  VALUE ZEROES.
           05 WSS-IND-P                  PIC 9(002)  VALUE ZEROES.
           05 WSS-SOMA                   PIC 9(008)  VALUE ZEROES.
           05 WSS-NUMERO                 PIC 9(015)  VALUE ZEROES.
           05 WSS-NUMERO-R               REDEFINES WSS-NUMERO.
              10  WSS-NUMERO-T           PIC 9(001)  OCCURS 15 TIMES.
           05 WSS-PESOS                  PIC X(028)  VALUE SPACES.
           05 WSS-PESOS-R                REDEFINES WSS-PESOS.
              10  WSS-PESOS-T            PIC 9(002)  OCCURS 14 TIMES.
           05 WSS-QUOCI                  PIC 9(008)  VALUE ZEROES.
           05 WSS-RESTO                  PIC 9(008)  VALUE ZEROES.
           05 WSS-MENSAGEM               PIC X(078)  VALUE SPACES.
           05 WSS-PESOS-CPF              PIC X(028)  VALUE
                                   '0000000011100908070605040302'.
           05 WSS-PESOS-CGC              PIC X(028)  VALUE
                                   '0706050403020908070605040302'.
           05 WSS-PESOS-PIS              PIC X(028)  VALUE
                                   '0000000003020908070605040302'.

       LINKAGE SECTION.
       01  LKS-PARAMETRO.
           05 COMPRIMENTO                PIC S9(04) COMP.
           05 LKS-NUMERO-I               PIC 9(015).
           05 FILLER                     PIC X(001).
           05 LKS-NUMERO-F               PIC 9(015).
           05 FILLER                     PIC X(001).
           05 LKS-TIPO-CALCULO           PIC X(003).
           05 FILLER                     PIC X(001).
           05 LKS-ACAO                   PIC X(001).
           05 LKS-RETORNO                PIC 9(001).

      ******************************************************************
      * LKS-NUMERO-I     = número da ser informado
      * LKS-NUMERO-F     = número retornado do programa
      * LKS-TIPO-CALCULO = CPF ou CGC ou PIS
      * LKS-ACAO         = C (calcula) V (verifica) 
      * LKS-RETORNO      = 0 - codigo verificado está correto
      *                  = 1 - LKS-TIPO-CALCULO está incorreto
      *                  = 2 - LKS-ACAO está incorreta
      *                  = 3 - código verificado está com erro	
      ******************************************************************
       
       PROCEDURE DIVISION USING LKS-PARAMETRO.

           PERFORM P1000-INICIAL   THRU P1000-FIM
           PERFORM P2000-PRINCIPAL THRU P2000-FIM
           PERFORM P9500-FINAL     THRU P9500-FIM
           GOBACK.

      *-----------------------------------------------------------------
       P1000-INICIAL.
      *-----------------------------------------------------------------

           MOVE ZEROES TO LKS-RETORNO  
           EVALUATE TRUE
              WHEN LKS-ACAO = 'C'
                   EVALUATE LKS-TIPO-CALCULO
                      WHEN 'CPF'
                         MOVE LKS-NUMERO-I (07:09) TO WSS-NUMERO (05:09)
                      WHEN 'CGC'
                         MOVE LKS-NUMERO-I (03:13) TO WSS-NUMERO (01:13)
                      WHEN 'PIS'
                         MOVE LKS-NUMERO-I (06:10) TO WSS-NUMERO (05:10)
                      WHEN OTHER
                         MOVE 1 TO LKS-RETORNO 
                         GOBACK
                   END-EVALUATE
              WHEN LKS-ACAO = 'V'
                   EVALUATE LKS-TIPO-CALCULO
                      WHEN 'CPF'
                      WHEN 'CGC'
                      WHEN 'PIS'
                         MOVE LKS-NUMERO-I TO WSS-NUMERO
                      WHEN OTHER
                         MOVE 1 TO LKS-RETORNO 
                         GOBACK
                   END-EVALUATE
              WHEN OTHER
                   MOVE 2 TO LKS-RETORNO 
                   GOBACK 
           END-EVALUATE.

       P1000-FIM.
           EXIT.

      *-----------------------------------------------------------------
       P2000-PRINCIPAL.
      *-----------------------------------------------------------------

           EVALUATE LKS-TIPO-CALCULO
              WHEN 'CPF'
                    PERFORM P2100-CALCULO-CPF THRU P2100-FIM
              WHEN 'CGC'
                    PERFORM P3100-CALCULO-CGC THRU P3100-FIM
              WHEN  OTHER
                    PERFORM P2400-CALCULO-PIS THRU P4100-FIM
           END-EVALUATE.

       P2000-FIM.
           EXIT.

      *-----------------------------------------------------------------
       P2100-CALCULO-CPF.
      *-----------------------------------------------------------------

           MOVE WSS-PESOS-CPF TO WSS-PESOS
           MOVE 05            TO WSS-IND-N
           MOVE 06            TO WSS-IND-P
           MOVE 13            TO WSS-IND-O
           MOVE ZEROES        TO WSS-SOMA
           PERFORM P7000-CALC-DIGITO-1 THRU P7000-FIM

           MOVE 05            TO WSS-IND-N
           MOVE 05            TO WSS-IND-P
           MOVE 14            TO WSS-IND-O
           MOVE ZEROES        TO WSS-SOMA
           PERFORM P8000-CALC-DIGITO-2 THRU P8000-FIM.


       P2100-FIM.
           EXIT.

      *-----------------------------------------------------------------
       P3100-CALCULO-CGC.
      *-----------------------------------------------------------------

           MOVE WSS-PESOS-CGC TO WSS-PESOS
           MOVE 01            TO WSS-IND-N
           MOVE 02            TO WSS-IND-P
           MOVE 13            TO WSS-IND-O
           MOVE ZEROES        TO WSS-SOMA
           PERFORM P7000-CALC-DIGITO-1 THRU P7000-FIM

           MOVE 01            TO WSS-IND-N
           MOVE 01            TO WSS-IND-P
           MOVE 14            TO WSS-IND-O
           MOVE ZEROES        TO WSS-SOMA
           PERFORM P8000-CALC-DIGITO-2 THRU P8000-FIM.

       P3100-FIM.
           EXIT.

      *-----------------------------------------------------------------
       P2400-CALCULO-PIS.
      *-----------------------------------------------------------------

           MOVE WSS-PESOS-PIS TO WSS-PESOS
           MOVE 05            TO WSS-IND-N
           MOVE 05            TO WSS-IND-P
           MOVE 14            TO WSS-IND-O
           MOVE ZEROES        TO WSS-SOMA
           PERFORM P8000-CALC-DIGITO-2 THRU P8000-FIM.

       P4100-FIM.
           EXIT.

      *-----------------------------------------------------------------
       P7000-CALC-DIGITO-1.
      *-----------------------------------------------------------------

           MOVE ZEROES TO WSS-SOMA
           PERFORM UNTIL WSS-IND-N GREATER WSS-IND-O
                   COMPUTE WSS-SOMA = WSS-SOMA +
                                     (WSS-NUMERO-T (WSS-IND-N) *
                                      WSS-PESOS-T  (WSS-IND-P))
                   ADD 1 TO WSS-IND-N
                            WSS-IND-P
           END-PERFORM
           DIVIDE WSS-SOMA BY 11 GIVING WSS-QUOCI REMAINDER WSS-RESTO
           IF WSS-RESTO EQUAL 0 OR 1
              MOVE ZEROES TO WSS-NUMERO-T (14)
           ELSE
              SUBTRACT WSS-RESTO FROM 11 GIVING WSS-NUMERO-T (14)
           END-IF.

       P7000-FIM.
           EXIT.

      *-----------------------------------------------------------------
       P8000-CALC-DIGITO-2.
      *-----------------------------------------------------------------

           MOVE ZEROES TO WSS-SOMA
           PERFORM UNTIL WSS-IND-N GREATER WSS-IND-O
                   COMPUTE WSS-SOMA = WSS-SOMA +
                                     (WSS-NUMERO-T (WSS-IND-N) *
                                      WSS-PESOS-T  (WSS-IND-P))
                   ADD 1 TO WSS-IND-N
                            WSS-IND-P
           END-PERFORM
           DIVIDE WSS-SOMA BY 11 GIVING WSS-QUOCI REMAINDER WSS-RESTO
           IF WSS-RESTO EQUAL 0 OR 1
              MOVE ZEROES TO WSS-NUMERO-T (15)
           ELSE
              SUBTRACT WSS-RESTO FROM 11 GIVING WSS-NUMERO-T (15)
           END-IF.

       P8000-FIM.
           EXIT.

      *-----------------------------------------------------------------
       P9500-FINAL.
      *-----------------------------------------------------------------

           MOVE WSS-NUMERO    TO LKS-NUMERO-F          
           IF  LKS-ACAO EQUAL 'V'                      
               IF LKS-NUMERO-I EQUAL LKS-NUMERO-F      
                  MOVE 0 TO LKS-RETORNO                
               ELSE                                    
                  MOVE 3 TO LKS-RETORNO                
               END-IF                                  
           ELSE                                        
               MOVE 0 TO LKS-RETORNO                   
           END-IF.                                      

       P9500-FIM.
           EXIT.

       END PROGRAM CALCDIGITO.